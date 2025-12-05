// Test import parsing
import { deriveEq, deriveHash } from "std/macros";
import { Eq } from "std/traits";
import DefaultExport from "./local_module";

// Use imported macros
@deriveEq
class User {
    id: number;
    name: string;
}
