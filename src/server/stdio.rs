use futures::future::{Empty, IntoStream};
use futures::sync::mpsc;
use futures::{future, Future, Sink, Stream};
use log::error;
use tokio::codec::{FramedRead, FramedWrite};
use tokio::io::{AsyncRead, AsyncWrite};
use tower::Service;

use super::codec::LanguageServerCodec;

#[derive(Debug)]
pub struct Server<I, O, S> {
    stdin: I,
    stdout: O,
    interleave: S,
}

impl<I, O> Server<I, O, IntoStream<Empty<String, ()>>>
where
    I: AsyncRead + Send + 'static,
    O: AsyncWrite + Send + 'static,
{
    pub fn new(stdin: I, stdout: O) -> Self {
        Server {
            stdin,
            stdout,
            interleave: future::empty().into_stream(),
        }
    }
}

impl<I, O, S> Server<I, O, S>
where
    I: AsyncRead + Send + 'static,
    O: AsyncWrite + Send + 'static,
    S: Stream<Item = String, Error = ()> + Send + 'static,
{
    pub fn interleave<T>(self, stream: T) -> Server<I, O, T>
    where
        T: Stream<Item = String, Error = ()> + Send + 'static,
    {
        Server {
            stdin: self.stdin,
            stdout: self.stdout,
            interleave: stream,
        }
    }

    pub fn serve<T>(self, service: T) -> impl Future<Item = (), Error = ()> + Send + 'static
    where
        T: Service<String, Response = String> + Send + 'static,
        T::Future: Send + 'static,
    {
        let (sender, receiver) = mpsc::channel(1);

        let framed_stdin = FramedRead::new(self.stdin, LanguageServerCodec::default());
        let framed_stdout = FramedWrite::new(self.stdout, LanguageServerCodec::default());
        let interleave = self.interleave;

        future::lazy(move || {
            let printer = receiver
                .select(interleave)
                .map_err(|_| error!("failed to log message"))
                .forward(framed_stdout.sink_map_err(|e| error!("failed to encode response: {}", e)))
                .map(|_| ());

            tokio::spawn(printer);

            framed_stdin
                .map_err(|e| error!("failed to decode request: {}", e))
                .fold(service, move |mut service, line| {
                    let sender = sender.clone();
                    service
                        .call(line)
                        .and_then(move |resp| sender.send(resp).map_err(|_| unreachable!()))
                        .then(move |_| Ok(service))
                })
                .map(|_| ())
        })
    }
}
