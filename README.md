# entity

## External dependencies

- black (`pip install black`)
- prettier (`npm install prettier -g`)

## Goals

- From entity description, generate entity classes and entity managers. (Django, TypeScript)
  - Generate code via AST library?
- Generate communication methods between frameworks. (REST)
- Support for languages/frameworks must be extendible. (SQL, Java, JavaScript)
- Support for communication methods must be extendible. (WebSockets)
- Must support adding of custom methods. (Q: add them within the entity description versus manually within the generated code?)
- Configurable: specify where code must be generated.
- Command line support for updating entities.
- Support for migrations. (only necessary for persistent frameworks like SQL?)
- Support for authorization.

## Example

Notes:

- somehow keep track of Python dependencies and generate right imports?
- mapping from entity field to concrete field: explicitely given, type map, default field for type
- field configuration: max characters etc -> also work with defaults and fallbacks (same hierarchy as field-mapping)
- provide default filename
- how to specify whether entities should be in seperate files or not? (might also not always be possible! -> language dependent)
- how to handle multiple generations for one entity? (e.g. JSONEvent, Event, InitialEvent, each with its own field-mapping??)
- handle renaming of variables (default, specify yourself) -> camelCase, pascal_case, ... (use common casing scripts?)
- language-specific options (e.g. Java: package)
- support optional fields?

- get example with relations!

### Entity

Event
name: String
year: Integer
description: String

Period
name: String
start: Integer
end: Integer
description: String

### Generation config

Python
filename = entities.py  // Optional (define resulting)
location = ./app/       // Mandatory
type-map = [            // Optional
  // example: String -> TextField()
]

Python-Event              // Optional
description = TextField() // Optional: overwrite field

Python-Period             // Optional
description = TextField() // Optional: overwrite field

TypeScript
location = ./frontend/src/app/api/
