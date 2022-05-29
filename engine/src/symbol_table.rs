// Symbol Table - HashMap for (token_name, meta data like datatypes etc.). Each scope bounded by {...} is represented by
// individual symbol table. Each symbol table points to the symbol table belonging to it's enclosing scope.
// Point of interactions:
// 1. Before lexical analysis phase to initialize the map with keywords.
// 2. During lexical analysis phase to distringuish between pattern matched for identifiers and keywords (if we found lexeme in
// symbol_table then it's a keyword so form token accordingly).
// 3. During parsing phase, we add entries in the table for identifiers when we find declartion statements in the current scope.
// 4. During parsing phase, when we encounter any statement involving identifiers then we access it for checking declartions, 
// datatypes etc.