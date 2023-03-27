#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct EnsureReadyRequest {}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct EnsureReadyResponse {}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GenerateSignatureRequest {
    /// The root of the workspace that the Tricorder will be analyzing.
    ///
    #[prost(string, tag = "1")]
    pub workspace_root: ::prost::alloc::string::String,
    /// The path to the file that this signature corresponds to. This path will be
    /// relative to the `workspace_root`.
    ///
    #[prost(string, tag = "2")]
    pub file: ::prost::alloc::string::String,
    /// The symbol that the generated signature should include.
    ///
    /// This symbol is a string that matches a value that is semantically
    /// meaningful in the context of the tricorder's language. For example, in the
    /// Erlang tricorder we expect a symbol to be "module", "function", "macro",
    /// or "type".
    ///
    #[prost(message, optional, tag = "3")]
    pub symbol: ::core::option::Option<super::Symbol>,
    /// A list of dependencies known to be required to generate a signature for
    /// this file.
    ///
    #[prost(message, repeated, tag = "4")]
    pub dependencies: ::prost::alloc::vec::Vec<super::Dependency>,
    #[prost(message, optional, tag = "5")]
    pub test_matcher: ::core::option::Option<super::TestMatcher>,
    #[prost(enumeration = "super::Goal", tag = "6")]
    pub goal: i32,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GenerateSignatureResponse {
    #[prost(oneof = "generate_signature_response::Response", tags = "1, 3")]
    pub response: ::core::option::Option<generate_signature_response::Response>,
}
/// Nested message and enum types in `GenerateSignatureResponse`.
pub mod generate_signature_response {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Response {
        #[prost(message, tag = "1")]
        Ok(super::GenerateSignatureSuccessResponse),
        #[prost(message, tag = "3")]
        MissingDeps(super::GenerateSignatureMissingDepsResponse),
    }
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GenerateSignatureSuccessResponse {
    /// The root of the workspace that was analyzed.
    #[prost(string, tag = "1")]
    pub workspace_root: ::prost::alloc::string::String,
    /// The file that the signatures correspod to.
    #[prost(string, tag = "2")]
    pub file: ::prost::alloc::string::String,
    /// The symbol within that file that the signatures correspond to.
    #[prost(message, optional, tag = "3")]
    pub symbol: ::core::option::Option<super::Symbol>,
    /// A non-empty collection of signatures for this file.
    #[prost(message, repeated, tag = "4")]
    pub signatures: ::prost::alloc::vec::Vec<super::Signature>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GenerateSignatureMissingDepsResponse {
    #[prost(string, tag = "1")]
    pub workspace_root: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub file: ::prost::alloc::string::String,
    #[prost(message, optional, tag = "3")]
    pub symbol: ::core::option::Option<super::Symbol>,
    /// The dependencies that were provided for signature generation.
    #[prost(message, repeated, tag = "4")]
    pub dependencies: ::prost::alloc::vec::Vec<super::Dependency>,
    /// A non-empty collection of requirements. These specify new dependencies
    /// that are needed to generate the signature.
    ///
    #[prost(message, repeated, tag = "5")]
    pub requirements: ::prost::alloc::vec::Vec<super::Requirement>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetAstRequest {
    #[prost(string, tag = "1")]
    pub file: ::prost::alloc::string::String,
    #[prost(message, optional, tag = "2")]
    pub symbol: ::core::option::Option<super::Symbol>,
    #[prost(message, repeated, tag = "3")]
    pub dependencies: ::prost::alloc::vec::Vec<super::Dependency>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetAstResponse {
    #[prost(oneof = "get_ast_response::Response", tags = "1, 3")]
    pub response: ::core::option::Option<get_ast_response::Response>,
}
/// Nested message and enum types in `GetAstResponse`.
pub mod get_ast_response {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Response {
        #[prost(message, tag = "1")]
        Ok(super::GetAstSuccessResponse),
        #[prost(message, tag = "3")]
        MissingDeps(super::GetAstMissingDepsResponse),
    }
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetAstMissingDepsResponse {
    #[prost(string, tag = "1")]
    pub file: ::prost::alloc::string::String,
    #[prost(message, optional, tag = "2")]
    pub symbol: ::core::option::Option<super::Symbol>,
    #[prost(message, repeated, tag = "3")]
    pub dependencies: ::prost::alloc::vec::Vec<super::Requirement>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetAstSuccessResponse {
    #[prost(string, tag = "1")]
    pub file: ::prost::alloc::string::String,
    #[prost(message, optional, tag = "2")]
    pub symbol: ::core::option::Option<super::Symbol>,
    #[prost(string, tag = "3")]
    pub source: ::prost::alloc::string::String,
    #[prost(string, tag = "4")]
    pub ast: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct PrepareDependencyRequest {
    /// The root directory where this package is located.
    #[prost(string, tag = "1")]
    pub package_root: ::prost::alloc::string::String,
    /// The URL that this package was downloded from.
    #[prost(string, tag = "2")]
    pub url: ::prost::alloc::string::String,
    /// The name of the dependency to preprae.
    #[prost(string, tag = "3")]
    pub package_name: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct PrepareDependencyResponse {
    #[prost(message, repeated, tag = "2")]
    pub signatures: ::prost::alloc::vec::Vec<super::Signature>,
}
/// Generated client implementations.
pub mod tricorder_service_client {
    #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
    use tonic::codegen::*;
    use tonic::codegen::http::Uri;
    /// A TricorderService is a repository and code analysis service created for a
    /// specific language ecosystem that will be started and managed from within
    /// Warp. It helps Warp understand how an ecosystem works.
    ///
    #[derive(Debug, Clone)]
    pub struct TricorderServiceClient<T> {
        inner: tonic::client::Grpc<T>,
    }
    impl TricorderServiceClient<tonic::transport::Channel> {
        /// Attempt to create a new client by connecting to a given endpoint.
        pub async fn connect<D>(dst: D) -> Result<Self, tonic::transport::Error>
        where
            D: std::convert::TryInto<tonic::transport::Endpoint>,
            D::Error: Into<StdError>,
        {
            let conn = tonic::transport::Endpoint::new(dst)?.connect().await?;
            Ok(Self::new(conn))
        }
    }
    impl<T> TricorderServiceClient<T>
    where
        T: tonic::client::GrpcService<tonic::body::BoxBody>,
        T::Error: Into<StdError>,
        T::ResponseBody: Body<Data = Bytes> + Send + 'static,
        <T::ResponseBody as Body>::Error: Into<StdError> + Send,
    {
        pub fn new(inner: T) -> Self {
            let inner = tonic::client::Grpc::new(inner);
            Self { inner }
        }
        pub fn with_origin(inner: T, origin: Uri) -> Self {
            let inner = tonic::client::Grpc::with_origin(inner, origin);
            Self { inner }
        }
        pub fn with_interceptor<F>(
            inner: T,
            interceptor: F,
        ) -> TricorderServiceClient<InterceptedService<T, F>>
        where
            F: tonic::service::Interceptor,
            T::ResponseBody: Default,
            T: tonic::codegen::Service<
                http::Request<tonic::body::BoxBody>,
                Response = http::Response<
                    <T as tonic::client::GrpcService<tonic::body::BoxBody>>::ResponseBody,
                >,
            >,
            <T as tonic::codegen::Service<
                http::Request<tonic::body::BoxBody>,
            >>::Error: Into<StdError> + Send + Sync,
        {
            TricorderServiceClient::new(InterceptedService::new(inner, interceptor))
        }
        /// Compress requests with the given encoding.
        ///
        /// This requires the server to support it otherwise it might respond with an
        /// error.
        #[must_use]
        pub fn send_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.inner = self.inner.send_compressed(encoding);
            self
        }
        /// Enable decompressing responses.
        #[must_use]
        pub fn accept_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.inner = self.inner.accept_compressed(encoding);
            self
        }
        /// Returns whether the service is ready to take further requests.
        ///
        /// Since we are initializing tricorder services from within warp as separate
        /// processes, we need a signal to tell us that it is indeed ready.
        ///
        pub async fn ensure_ready(
            &mut self,
            request: impl tonic::IntoRequest<super::EnsureReadyRequest>,
        ) -> Result<tonic::Response<super::EnsureReadyResponse>, tonic::Status> {
            self.inner
                .ready()
                .await
                .map_err(|e| {
                    tonic::Status::new(
                        tonic::Code::Unknown,
                        format!("Service was not ready: {}", e.into()),
                    )
                })?;
            let codec = tonic::codec::ProstCodec::default();
            let path = http::uri::PathAndQuery::from_static(
                "/build.warp.tricorder.TricorderService/EnsureReady",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
        /// Generates a Warp Signature for a given source and symbol. This is used
        /// during the Target Resolution phase, to understand how to build, test, or
        /// run a given target.
        ///
        /// The entirety of the repository is normally available to this rpc, but the
        /// signature is expected to correspond to a single path, be it a file or a
        /// directory.
        ///
        /// A list of dependencies will be provided to facilitate the analysis of code
        /// in case that paths to 3rd party dependencies are required. For example, in
        /// languages with preprocessors that require the inclusion of header files,
        /// this list of dependencies will include information about the paths where
        /// those header files may reside.
        ///
        pub async fn generate_signature(
            &mut self,
            request: impl tonic::IntoRequest<super::GenerateSignatureRequest>,
        ) -> Result<tonic::Response<super::GenerateSignatureResponse>, tonic::Status> {
            self.inner
                .ready()
                .await
                .map_err(|e| {
                    tonic::Status::new(
                        tonic::Code::Unknown,
                        format!("Service was not ready: {}", e.into()),
                    )
                })?;
            let codec = tonic::codec::ProstCodec::default();
            let path = http::uri::PathAndQuery::from_static(
                "/build.warp.tricorder.TricorderService/GenerateSignature",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
        pub async fn get_ast(
            &mut self,
            request: impl tonic::IntoRequest<super::GetAstRequest>,
        ) -> Result<tonic::Response<super::GetAstResponse>, tonic::Status> {
            self.inner
                .ready()
                .await
                .map_err(|e| {
                    tonic::Status::new(
                        tonic::Code::Unknown,
                        format!("Service was not ready: {}", e.into()),
                    )
                })?;
            let codec = tonic::codec::ProstCodec::default();
            let path = http::uri::PathAndQuery::from_static(
                "/build.warp.tricorder.TricorderService/GetAst",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
        pub async fn prepare_dependency(
            &mut self,
            request: impl tonic::IntoRequest<super::PrepareDependencyRequest>,
        ) -> Result<tonic::Response<super::PrepareDependencyResponse>, tonic::Status> {
            self.inner
                .ready()
                .await
                .map_err(|e| {
                    tonic::Status::new(
                        tonic::Code::Unknown,
                        format!("Service was not ready: {}", e.into()),
                    )
                })?;
            let codec = tonic::codec::ProstCodec::default();
            let path = http::uri::PathAndQuery::from_static(
                "/build.warp.tricorder.TricorderService/PrepareDependency",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
    }
}
/// Generated server implementations.
pub mod tricorder_service_server {
    #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
    use tonic::codegen::*;
    /// Generated trait containing gRPC methods that should be implemented for use with TricorderServiceServer.
    #[async_trait]
    pub trait TricorderService: Send + Sync + 'static {
        /// Returns whether the service is ready to take further requests.
        ///
        /// Since we are initializing tricorder services from within warp as separate
        /// processes, we need a signal to tell us that it is indeed ready.
        ///
        async fn ensure_ready(
            &self,
            request: tonic::Request<super::EnsureReadyRequest>,
        ) -> Result<tonic::Response<super::EnsureReadyResponse>, tonic::Status>;
        /// Generates a Warp Signature for a given source and symbol. This is used
        /// during the Target Resolution phase, to understand how to build, test, or
        /// run a given target.
        ///
        /// The entirety of the repository is normally available to this rpc, but the
        /// signature is expected to correspond to a single path, be it a file or a
        /// directory.
        ///
        /// A list of dependencies will be provided to facilitate the analysis of code
        /// in case that paths to 3rd party dependencies are required. For example, in
        /// languages with preprocessors that require the inclusion of header files,
        /// this list of dependencies will include information about the paths where
        /// those header files may reside.
        ///
        async fn generate_signature(
            &self,
            request: tonic::Request<super::GenerateSignatureRequest>,
        ) -> Result<tonic::Response<super::GenerateSignatureResponse>, tonic::Status>;
        async fn get_ast(
            &self,
            request: tonic::Request<super::GetAstRequest>,
        ) -> Result<tonic::Response<super::GetAstResponse>, tonic::Status>;
        async fn prepare_dependency(
            &self,
            request: tonic::Request<super::PrepareDependencyRequest>,
        ) -> Result<tonic::Response<super::PrepareDependencyResponse>, tonic::Status>;
    }
    /// A TricorderService is a repository and code analysis service created for a
    /// specific language ecosystem that will be started and managed from within
    /// Warp. It helps Warp understand how an ecosystem works.
    ///
    #[derive(Debug)]
    pub struct TricorderServiceServer<T: TricorderService> {
        inner: _Inner<T>,
        accept_compression_encodings: EnabledCompressionEncodings,
        send_compression_encodings: EnabledCompressionEncodings,
    }
    struct _Inner<T>(Arc<T>);
    impl<T: TricorderService> TricorderServiceServer<T> {
        pub fn new(inner: T) -> Self {
            Self::from_arc(Arc::new(inner))
        }
        pub fn from_arc(inner: Arc<T>) -> Self {
            let inner = _Inner(inner);
            Self {
                inner,
                accept_compression_encodings: Default::default(),
                send_compression_encodings: Default::default(),
            }
        }
        pub fn with_interceptor<F>(
            inner: T,
            interceptor: F,
        ) -> InterceptedService<Self, F>
        where
            F: tonic::service::Interceptor,
        {
            InterceptedService::new(Self::new(inner), interceptor)
        }
        /// Enable decompressing requests with the given encoding.
        #[must_use]
        pub fn accept_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.accept_compression_encodings.enable(encoding);
            self
        }
        /// Compress responses with the given encoding, if the client supports it.
        #[must_use]
        pub fn send_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.send_compression_encodings.enable(encoding);
            self
        }
    }
    impl<T, B> tonic::codegen::Service<http::Request<B>> for TricorderServiceServer<T>
    where
        T: TricorderService,
        B: Body + Send + 'static,
        B::Error: Into<StdError> + Send + 'static,
    {
        type Response = http::Response<tonic::body::BoxBody>;
        type Error = std::convert::Infallible;
        type Future = BoxFuture<Self::Response, Self::Error>;
        fn poll_ready(
            &mut self,
            _cx: &mut Context<'_>,
        ) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }
        fn call(&mut self, req: http::Request<B>) -> Self::Future {
            let inner = self.inner.clone();
            match req.uri().path() {
                "/build.warp.tricorder.TricorderService/EnsureReady" => {
                    #[allow(non_camel_case_types)]
                    struct EnsureReadySvc<T: TricorderService>(pub Arc<T>);
                    impl<
                        T: TricorderService,
                    > tonic::server::UnaryService<super::EnsureReadyRequest>
                    for EnsureReadySvc<T> {
                        type Response = super::EnsureReadyResponse;
                        type Future = BoxFuture<
                            tonic::Response<Self::Response>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::EnsureReadyRequest>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move {
                                (*inner).ensure_ready(request).await
                            };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = EnsureReadySvc(inner);
                        let codec = tonic::codec::ProstCodec::default();
                        let mut grpc = tonic::server::Grpc::new(codec)
                            .apply_compression_config(
                                accept_compression_encodings,
                                send_compression_encodings,
                            );
                        let res = grpc.unary(method, req).await;
                        Ok(res)
                    };
                    Box::pin(fut)
                }
                "/build.warp.tricorder.TricorderService/GenerateSignature" => {
                    #[allow(non_camel_case_types)]
                    struct GenerateSignatureSvc<T: TricorderService>(pub Arc<T>);
                    impl<
                        T: TricorderService,
                    > tonic::server::UnaryService<super::GenerateSignatureRequest>
                    for GenerateSignatureSvc<T> {
                        type Response = super::GenerateSignatureResponse;
                        type Future = BoxFuture<
                            tonic::Response<Self::Response>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::GenerateSignatureRequest>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move {
                                (*inner).generate_signature(request).await
                            };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = GenerateSignatureSvc(inner);
                        let codec = tonic::codec::ProstCodec::default();
                        let mut grpc = tonic::server::Grpc::new(codec)
                            .apply_compression_config(
                                accept_compression_encodings,
                                send_compression_encodings,
                            );
                        let res = grpc.unary(method, req).await;
                        Ok(res)
                    };
                    Box::pin(fut)
                }
                "/build.warp.tricorder.TricorderService/GetAst" => {
                    #[allow(non_camel_case_types)]
                    struct GetAstSvc<T: TricorderService>(pub Arc<T>);
                    impl<
                        T: TricorderService,
                    > tonic::server::UnaryService<super::GetAstRequest>
                    for GetAstSvc<T> {
                        type Response = super::GetAstResponse;
                        type Future = BoxFuture<
                            tonic::Response<Self::Response>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::GetAstRequest>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move { (*inner).get_ast(request).await };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = GetAstSvc(inner);
                        let codec = tonic::codec::ProstCodec::default();
                        let mut grpc = tonic::server::Grpc::new(codec)
                            .apply_compression_config(
                                accept_compression_encodings,
                                send_compression_encodings,
                            );
                        let res = grpc.unary(method, req).await;
                        Ok(res)
                    };
                    Box::pin(fut)
                }
                "/build.warp.tricorder.TricorderService/PrepareDependency" => {
                    #[allow(non_camel_case_types)]
                    struct PrepareDependencySvc<T: TricorderService>(pub Arc<T>);
                    impl<
                        T: TricorderService,
                    > tonic::server::UnaryService<super::PrepareDependencyRequest>
                    for PrepareDependencySvc<T> {
                        type Response = super::PrepareDependencyResponse;
                        type Future = BoxFuture<
                            tonic::Response<Self::Response>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::PrepareDependencyRequest>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move {
                                (*inner).prepare_dependency(request).await
                            };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = PrepareDependencySvc(inner);
                        let codec = tonic::codec::ProstCodec::default();
                        let mut grpc = tonic::server::Grpc::new(codec)
                            .apply_compression_config(
                                accept_compression_encodings,
                                send_compression_encodings,
                            );
                        let res = grpc.unary(method, req).await;
                        Ok(res)
                    };
                    Box::pin(fut)
                }
                _ => {
                    Box::pin(async move {
                        Ok(
                            http::Response::builder()
                                .status(200)
                                .header("grpc-status", "12")
                                .header("content-type", "application/grpc")
                                .body(empty_body())
                                .unwrap(),
                        )
                    })
                }
            }
        }
    }
    impl<T: TricorderService> Clone for TricorderServiceServer<T> {
        fn clone(&self) -> Self {
            let inner = self.inner.clone();
            Self {
                inner,
                accept_compression_encodings: self.accept_compression_encodings,
                send_compression_encodings: self.send_compression_encodings,
            }
        }
    }
    impl<T: TricorderService> Clone for _Inner<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
        }
    }
    impl<T: std::fmt::Debug> std::fmt::Debug for _Inner<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.0)
        }
    }
    impl<T: TricorderService> tonic::server::NamedService for TricorderServiceServer<T> {
        const NAME: &'static str = "build.warp.tricorder.TricorderService";
    }
}
