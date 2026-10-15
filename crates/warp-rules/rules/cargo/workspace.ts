interface CargoDependency {
  name: string;
  pkg: string;
  dep_kinds: Array<{ kind: string | null }>;
}

interface CargoTarget {
  name: string;
  kind: string[];
  src_path: string;
  "required-features"?: string[];
}

interface CargoPackage {
  id: string;
  name: string;
  version: string;
  manifest_path: string;
  edition: string;
  targets: CargoTarget[];
}

interface CargoMetadata {
  workspace_root: string;
  target_directory: string;
  workspace_members: string[];
  packages: CargoPackage[];
  resolve: {
    nodes: Array<{
      id: string;
      features?: string[];
      deps?: CargoDependency[];
    }>;
  } | null;
}

interface WorkspaceRuleConfiguration {
  metadata_json: string;
}

declare const configuration: WorkspaceRuleConfiguration;
declare const Warp: { emit(plan: unknown): void };

const metadata = JSON.parse(configuration.metadata_json) as CargoMetadata;
if (metadata.resolve === null) {
  throw new Error("Cargo metadata did not include a resolved dependency graph");
}
const nodes = new Map(metadata.resolve.nodes.map((node) => [node.id, node]));

const packages = metadata.packages.map((pkg) => {
  const node = nodes.get(pkg.id);
  return {
    id: pkg.id,
    name: pkg.name,
    version: pkg.version,
    manifest_path: pkg.manifest_path,
    edition: pkg.edition,
    features: node?.features ?? [],
    targets: pkg.targets.map((target) => {
      const kind = target.kind.includes("custom-build")
        ? "custom_build"
        : target.kind.includes("proc-macro")
        ? "proc_macro"
        : target.kind.includes("lib")
        ? "library"
        : target.kind.includes("bin")
        ? "binary"
        : "other";
      return {
        name: target.name,
        kind,
        source_path: target.src_path,
        required_features: target["required-features"] ?? [],
      };
    }),
    dependencies: (node?.deps ?? [])
      .filter((dependency) => dependency.dep_kinds.some((kind) => kind.kind === null))
      .map((dependency) => ({ alias: dependency.name, package_id: dependency.pkg })),
  };
});

Warp.emit({
  workspace_root: metadata.workspace_root,
  artifact_directory: metadata.target_directory,
  workspace_members: metadata.workspace_members,
  packages,
});
