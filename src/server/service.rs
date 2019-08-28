use std::fmt::Display;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use futures::future::{self, Future, Shared, SharedError, SharedItem};
use futures::sink::Sink;
use futures::stream::Stream;
use futures::sync::mpsc;
use futures::sync::oneshot::{self, Canceled};
use futures::{Async, Poll};
use jsonrpc_core::IoHandler;
use log::{error, info, trace};
use lsp_types::{Diagnostic, LogMessageParams, MessageType, PublishDiagnosticsParams, Url};
use serde::Serialize;
use tower::Service;

use super::delegate::{Delegate, LanguageServerCore};
use super::LanguageServer;

#[derive(Clone, Debug)]
pub struct ExitReceiver(Shared<oneshot::Receiver<()>>);

impl ExitReceiver {
    pub fn run_until_exit<F>(self, future: F) -> impl Future<Item = (), Error = ()> + Send + 'static
    where
        F: Future<Item = (), Error = ()> + Send + 'static,
    {
        self.0.then(|_| Ok(())).select(future).then(|_| Ok(()))
    }
}

impl Future for ExitReceiver {
    type Item = SharedItem<()>;
    type Error = SharedError<Canceled>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        self.0.poll()
    }
}

#[derive(Debug)]
pub struct MessageStream(mpsc::Receiver<String>);

impl Stream for MessageStream {
    type Item = String;
    type Error = ();

    fn poll(&mut self) -> Poll<Option<String>, ()> {
        self.0.poll()
    }
}

#[derive(Clone, Debug)]
pub struct Printer(mpsc::Sender<String>);

impl Printer {
    pub fn log_message<M: Display>(&self, typ: MessageType, message: M) {
        self.send_notification(
            "window/logMessage",
            LogMessageParams {
                typ,
                message: message.to_string(),
            },
        );
    }

    pub fn publish_diagnostics(&self, uri: Url, diagnostics: Vec<Diagnostic>) {
        let params = PublishDiagnosticsParams::new(uri, diagnostics);
        self.send_notification("textDocument/publishDiagnostics", params);
    }

    fn send_notification<S: Serialize>(&self, method: &str, params: S) {
        match serde_json::to_string(&params) {
            Err(err) => error!("failed to serialize message for `{}`: {}", method, err),
            Ok(params) => {
                let message = format!(
                    r#"{{"jsonrpc":"2.0","method":"{}","params":{}}}"#,
                    method, params
                );
                tokio::spawn(
                    self.0
                        .clone()
                        .send(message)
                        .map(|_| ())
                        .map_err(|_| error!("failed to send message")),
                );
            }
        }
    }
}

#[derive(Debug)]
pub struct LspService {
    handler: IoHandler,
    exit_rx: ExitReceiver,
    stopped: Arc<AtomicBool>,
}

impl LspService {
    pub fn new<T>(server: T) -> (Self, MessageStream)
    where
        T: LanguageServer,
    {
        Self::with_handler(server, IoHandler::new())
    }

    pub fn with_handler<T, U>(server: T, handler: U) -> (Self, MessageStream)
    where
        T: LanguageServer,
        U: Into<IoHandler>,
    {
        let (tx, rx) = mpsc::channel(1);
        let print_tx = Printer(tx);
        let print_rx = MessageStream(rx);

        let mut handler = handler.into();
        handler.extend_with(Delegate::new(server, print_tx).to_delegate());

        let (tx, rx) = oneshot::channel();
        let exit_tx = Mutex::new(Some(tx));
        let exit_rx = ExitReceiver(rx.shared());

        let stopped = Arc::new(AtomicBool::new(false));
        let stopped_arc = stopped.clone();
        handler.add_notification("exit", move |_| {
            if let Some(tx) = exit_tx.lock().unwrap_or_else(|tx| tx.into_inner()).take() {
                info!("exit notification received, shutting down");
                stopped_arc.store(true, Ordering::SeqCst);
                tx.send(()).unwrap();
            }
        });

        let service = LspService {
            handler,
            exit_rx,
            stopped,
        };

        (service, print_rx)
    }

    pub fn close_handle(&self) -> ExitReceiver {
        self.exit_rx.clone()
    }
}

impl Service<String> for LspService {
    type Response = String;
    type Error = ();
    type Future = Box<dyn Future<Item = Self::Response, Error = Self::Error> + Send>;

    fn poll_ready(&mut self) -> Poll<(), Self::Error> {
        if self.stopped.load(Ordering::SeqCst) {
            Ok(Async::NotReady)
        } else {
            Ok(Async::Ready(()))
        }
    }

    fn call(&mut self, request: String) -> Self::Future {
        if self.stopped.load(Ordering::SeqCst) {
            Box::new(future::err(()))
        } else {
            Box::new(self.handler.handle_request(&request).map(move |result| {
                result.unwrap_or_else(|| {
                    trace!("request produced no response: {}", request);
                    String::new()
                })
            }))
        }
    }
}
