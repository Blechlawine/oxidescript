import module from "./module";
import { submodule_function } from "./module/submodule";

function main() {
	submodule_function();
	console.log("test");
	module.hello();
}

main();
