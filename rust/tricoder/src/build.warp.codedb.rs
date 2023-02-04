#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetDependenciesRequest {
    #[prost(string, tag = "1")]
    pub workspace_root: ::prost::alloc::string::String,
    #[prost(string, repeated, tag = "2")]
    pub profiles: ::prost::alloc::vec::Vec<::prost::alloc::string::String>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetDependenciesResponse {
    #[prost(enumeration = "super::Status", tag = "1")]
    pub status: i32,
    #[prost(message, repeated, tag = "2")]
    pub dependencies: ::prost::alloc::vec::Vec<super::Dependency>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GenerateSignatureRequest {
    #[prost(string, tag = "1")]
    pub file: ::prost::alloc::string::String,
    #[prost(message, repeated, tag = "2")]
    pub dependencies: ::prost::alloc::vec::Vec<super::Dependency>,
    #[prost(message, optional, tag = "3")]
    pub symbol: ::core::option::Option<super::Symbol>,
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
    #[prost(enumeration = "super::Status", tag = "1")]
    pub status: i32,
    #[prost(string, tag = "2")]
    pub file: ::prost::alloc::string::String,
    #[prost(string, tag = "3")]
    pub json_signature: ::prost::alloc::string::String,
    #[prost(message, repeated, tag = "4")]
    pub signatures: ::prost::alloc::vec::Vec<super::Signature>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GenerateSignatureMissingDepsResponse {
    #[prost(string, tag = "1")]
    pub file: ::prost::alloc::string::String,
    #[prost(message, optional, tag = "2")]
    pub symbol: ::core::option::Option<super::Symbol>,
    #[prost(message, repeated, tag = "3")]
    pub dependencies: ::prost::alloc::vec::Vec<super::Requirement>,
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
pub struct GetInterestedPathsRequest {}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetInterestedPathsResponse {
    #[prost(string, repeated, tag = "1")]
    pub build_files: ::prost::alloc::vec::Vec<::prost::alloc::string::String>,
    #[prost(string, repeated, tag = "2")]
    pub test_files: ::prost::alloc::vec::Vec<::prost::alloc::string::String>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetProvidedSymbolsRequest {
    #[prost(string, tag = "1")]
    pub file: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct GetProvidedSymbolsResponse {
    #[prost(bool, tag = "1")]
    pub skipped: bool,
    #[prost(string, tag = "2")]
    pub file: ::prost::alloc::string::String,
    #[prost(message, repeated, tag = "3")]
    pub provides: ::prost::alloc::vec::Vec<super::Requirement>,
}
/// Generated client implementations.
pub mod analyzer_service_client {
    #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
    use tonic::codegen::*;
    use tonic::codegen::http::Uri;
    #[derive(Debug, Clone)]
    pub struct AnalyzerServiceClient<T> {
        inner: tonic::client::Grpc<T>,
    }
    impl AnalyzerServiceClient<tonic::transport::Channel> {
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
    impl<T> AnalyzerServiceClient<T>
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
        ) -> AnalyzerServiceClient<InterceptedService<T, F>>
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
            AnalyzerServiceClient::new(InterceptedService::new(inner, interceptor))
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
        pub async fn get_dependencies(
            &mut self,
            request: impl tonic::IntoRequest<super::GetDependenciesRequest>,
        ) -> Result<tonic::Response<super::GetDependenciesResponse>, tonic::Status> {
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
                "/build.warp.codedb.AnalyzerService/GetDependencies",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
        pub async fn get_interested_paths(
            &mut self,
            request: impl tonic::IntoRequest<super::GetInterestedPathsRequest>,
        ) -> Result<tonic::Response<super::GetInterestedPathsResponse>, tonic::Status> {
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
                "/build.warp.codedb.AnalyzerService/GetInterestedPaths",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
        pub async fn get_provided_symbols(
            &mut self,
            request: impl tonic::IntoRequest<super::GetProvidedSymbolsRequest>,
        ) -> Result<tonic::Response<super::GetProvidedSymbolsResponse>, tonic::Status> {
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
                "/build.warp.codedb.AnalyzerService/GetProvidedSymbols",
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
                "/build.warp.codedb.AnalyzerService/GetAst",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
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
                "/build.warp.codedb.AnalyzerService/GenerateSignature",
            );
            self.inner.unary(request.into_request(), path, codec).await
        }
    }
}
/// Generated server implementations.
pub mod analyzer_service_server {
    #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
    use tonic::codegen::*;
    /// Generated trait containing gRPC methods that should be implemented for use with AnalyzerServiceServer.
    #[async_trait]
    pub trait AnalyzerService: Send + Sync + 'static {
        async fn get_dependencies(
            &self,
            request: tonic::Request<super::GetDependenciesRequest>,
        ) -> Result<tonic::Response<super::GetDependenciesResponse>, tonic::Status>;
        async fn get_interested_paths(
            &self,
            request: tonic::Request<super::GetInterestedPathsRequest>,
        ) -> Result<tonic::Response<super::GetInterestedPathsResponse>, tonic::Status>;
        async fn get_provided_symbols(
            &self,
            request: tonic::Request<super::GetProvidedSymbolsRequest>,
        ) -> Result<tonic::Response<super::GetProvidedSymbolsResponse>, tonic::Status>;
        async fn get_ast(
            &self,
            request: tonic::Request<super::GetAstRequest>,
        ) -> Result<tonic::Response<super::GetAstResponse>, tonic::Status>;
        async fn generate_signature(
            &self,
            request: tonic::Request<super::GenerateSignatureRequest>,
        ) -> Result<tonic::Response<super::GenerateSignatureResponse>, tonic::Status>;
    }
    #[derive(Debug)]
    pub struct AnalyzerServiceServer<T: AnalyzerService> {
        inner: _Inner<T>,
        accept_compression_encodings: EnabledCompressionEncodings,
        send_compression_encodings: EnabledCompressionEncodings,
    }
    struct _Inner<T>(Arc<T>);
    impl<T: AnalyzerService> AnalyzerServiceServer<T> {
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
    impl<T, B> tonic::codegen::Service<http::Request<B>> for AnalyzerServiceServer<T>
    where
        T: AnalyzerService,
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
                "/build.warp.codedb.AnalyzerService/GetDependencies" => {
                    #[allow(non_camel_case_types)]
                    struct GetDependenciesSvc<T: AnalyzerService>(pub Arc<T>);
                    impl<
                        T: AnalyzerService,
                    > tonic::server::UnaryService<super::GetDependenciesRequest>
                    for GetDependenciesSvc<T> {
                        type Response = super::GetDependenciesResponse;
                        type Future = BoxFuture<
                            tonic::Response<Self::Response>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::GetDependenciesRequest>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move {
                                (*inner).get_dependencies(request).await
                            };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = GetDependenciesSvc(inner);
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
                "/build.warp.codedb.AnalyzerService/GetInterestedPaths" => {
                    #[allow(non_camel_case_types)]
                    struct GetInterestedPathsSvc<T: AnalyzerService>(pub Arc<T>);
                    impl<
                        T: AnalyzerService,
                    > tonic::server::UnaryService<super::GetInterestedPathsRequest>
                    for GetInterestedPathsSvc<T> {
                        type Response = super::GetInterestedPathsResponse;
                        type Future = BoxFuture<
                            tonic::Response<Self::Response>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::GetInterestedPathsRequest>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move {
                                (*inner).get_interested_paths(request).await
                            };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = GetInterestedPathsSvc(inner);
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
                "/build.warp.codedb.AnalyzerService/GetProvidedSymbols" => {
                    #[allow(non_camel_case_types)]
                    struct GetProvidedSymbolsSvc<T: AnalyzerService>(pub Arc<T>);
                    impl<
                        T: AnalyzerService,
                    > tonic::server::UnaryService<super::GetProvidedSymbolsRequest>
                    for GetProvidedSymbolsSvc<T> {
                        type Response = super::GetProvidedSymbolsResponse;
                        type Future = BoxFuture<
                            tonic::Response<Self::Response>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::GetProvidedSymbolsRequest>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move {
                                (*inner).get_provided_symbols(request).await
                            };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = GetProvidedSymbolsSvc(inner);
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
                "/build.warp.codedb.AnalyzerService/GetAst" => {
                    #[allow(non_camel_case_types)]
                    struct GetAstSvc<T: AnalyzerService>(pub Arc<T>);
                    impl<
                        T: AnalyzerService,
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
                "/build.warp.codedb.AnalyzerService/GenerateSignature" => {
                    #[allow(non_camel_case_types)]
                    struct GenerateSignatureSvc<T: AnalyzerService>(pub Arc<T>);
                    impl<
                        T: AnalyzerService,
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
    impl<T: AnalyzerService> Clone for AnalyzerServiceServer<T> {
        fn clone(&self) -> Self {
            let inner = self.inner.clone();
            Self {
                inner,
                accept_compression_encodings: self.accept_compression_encodings,
                send_compression_encodings: self.send_compression_encodings,
            }
        }
    }
    impl<T: AnalyzerService> Clone for _Inner<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
        }
    }
    impl<T: std::fmt::Debug> std::fmt::Debug for _Inner<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.0)
        }
    }
    impl<T: AnalyzerService> tonic::server::NamedService for AnalyzerServiceServer<T> {
        const NAME: &'static str = "build.warp.codedb.AnalyzerService";
    }
}
