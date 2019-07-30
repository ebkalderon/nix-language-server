use futures::sync::mpsc;
use futures::{future, Future, Sink, Stream};
use log::error;
use tokio::codec::{FramedRead, FramedWrite};
use tokio::io::{AsyncRead, AsyncWrite};
use tower::Service;

use super::codec::LanguageServerCodec;

#[derive(Debug)]
pub struct Server<I, O> {
    stdin: I,
    stdout: O,
}

impl<I, O> Server<I, O>
where
    I: AsyncRead + Send + 'static,
    O: AsyncWrite + Send + 'static,
{
    pub fn new(stdin: I, stdout: O) -> Self {
        Server { stdin, stdout }
    }

    pub fn serve<S>(self, service: S) -> impl Future<Item = (), Error = ()> + Send + 'static
    where
        S: Service<String, Response = String> + Send + 'static,
        S::Future: Send + 'static,
    {
        let (sender, receiver) = mpsc::channel(1);

        let framed_stdin = FramedRead::new(self.stdin, LanguageServerCodec::default());
        let framed_stdout = FramedWrite::new(self.stdout, LanguageServerCodec::default());

        future::lazy(move || {
            let printer = receiver
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
