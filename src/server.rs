use std::sync::Arc;

use futures::future::{Future, Shared};
use futures::sync::mpsc;
use futures::sync::oneshot::{Receiver, Sender};
use futures::{future, Poll, Sink, Stream};
use jsonrpc_core::IoHandler;
use log::{error, trace};
use tokio::codec::{FramedRead, FramedWrite};
use tokio::io::{AsyncRead, AsyncWrite, Stdin, Stdout};
use tower::{Service, ServiceBuilder};

use self::codec::LanguageServerCodec;
use self::service::LspService;
use crate::backend::Server as LanguageServer;
use crate::Error;

mod codec;
mod service;

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

    pub fn serve(self) -> impl Future<Item = (), Error = ()> + Send + 'static {
        let service = LspService::new(LanguageServer);
        let exit_rx = service.close_handle().then(|_| Ok(()));
        self.serve_with(service).select(exit_rx).then(|_| Ok(()))
    }

    pub fn serve_with<S>(self, service: S) -> impl Future<Item = (), Error = ()> + Send + 'static
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
                    tokio::spawn(service.call(line).map_err(|_| ()).and_then(move |resp| {
                        sender.send(resp).map(|_| ()).map_err(|_| unreachable!())
                    }));

                    Ok(service)
                })
                .map(|_| ())
        })
    }
}
