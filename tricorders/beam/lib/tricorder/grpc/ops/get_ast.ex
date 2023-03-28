defmodule Tricorder.Grpc.Ops.GetAst do
  def get_ast(req, _stream) do
    srcs = File.read!(Path.join(req.workspace_root, req.file))
    result = Build.Warp.Tricorder.GetAstSuccessResponse.new(ast: srcs)
    Build.Warp.Tricorder.GetAstResponse.new(response: {:ok, result})
  end
end
