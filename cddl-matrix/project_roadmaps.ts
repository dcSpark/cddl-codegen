import { createNodeRoadmapCliPorts, runRoadmapCli } from "./roadmap/index.ts";

const ports = createNodeRoadmapCliPorts({ matrix_dir: import.meta.dir });
const result = runRoadmapCli(Bun.argv.slice(2), ports);

if (result.stdout.byteLength > 0) await Bun.write(Bun.stdout, result.stdout);
if (result.stderr.byteLength > 0) await Bun.write(Bun.stderr, result.stderr);
process.exitCode = result.exit_code;
