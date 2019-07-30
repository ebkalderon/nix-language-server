use std::sync::{Arc, Mutex};

use futures::future::{Future, Shared};
use futures::sync::oneshot::{self, Receiver, Sender};
use futures::Poll;
use jsonrpc_core::IoHandler;
use log::{info, trace};
use tower::Service;

use crate::protocol::LanguageServer;

pub type ExitReceiver = Shared<Receiver<()>>;

#[derive(Clone, Debug)]
pub struct LspService {
    handler: Arc<IoHandler>,
    exit_rx: ExitReceiver,
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
        handler.add_notification("exit", move |_| {
            if let Some(tx) = exit_tx.lock().unwrap_or_else(|tx| tx.into_inner()).take() {
                info!("exit notification received, shutting down");
                tx.send(()).unwrap()
            }
        });

        LspService {
            handler: Arc::new(handler),
            exit_rx: rx.shared(),
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
        Ok(().into())
    }

    fn call(&mut self, request: String) -> Self::Future {
        Box::new(self.handler.handle_request(&request).map(move |result| {
            result.unwrap_or_else(|| {
                trace!("request produced no response: {}", request);
                String::new()
            })
        }))
    }
}
