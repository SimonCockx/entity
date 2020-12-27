# entity

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
