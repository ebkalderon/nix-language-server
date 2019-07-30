use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use futures::future::{self, Future, Shared, SharedError, SharedItem};
use futures::sync::oneshot::{self, Canceled, Receiver};
use futures::{Async, Poll};
use jsonrpc_core::IoHandler;
use log::{info, trace};
use tower::Service;

use super::LanguageServer;

#[derive(Clone, Debug)]
pub struct ExitReceiver(Shared<Receiver<()>>);

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
pub struct LspService {
    handler: IoHandler,
    exit_rx: ExitReceiver,
    stopped: Arc<AtomicBool>,
}

impl LspService {
    pub fn new<T>(server: T) -> Self
    where
        T: LanguageServer,
    {
        Self::with_handler(server, IoHandler::new())
    }

    pub fn with_handler<T, U>(server: T, handler: U) -> Self
    where
        T: LanguageServer,
        U: Into<IoHandler>,
    {
        let (tx, rx) = oneshot::channel();

        let mut handler = handler.into();
        handler.extend_with(server.to_delegate());

        let exit_tx = Mutex::new(Some(tx));
        let stopped = Arc::new(AtomicBool::new(false));
        let stopped_arc = stopped.clone();
        handler.add_notification("exit", move |_| {
            if let Some(tx) = exit_tx.lock().unwrap_or_else(|tx| tx.into_inner()).take() {
                info!("exit notification received, shutting down");
                stopped_arc.store(true, Ordering::SeqCst);
                tx.send(()).unwrap();
            }
        });

        LspService {
            handler,
            exit_rx: ExitReceiver(rx.shared()),
            stopped,
        }
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
