(declare-project
  :name "csharp-fn-parser"
  :author "Gemini"
  :license "MIT"
  :description "A simple tool to find function calls in C# code using PEG."
  :version "0.1.0"
  :source ["src/main.janet"]
  :entry "main.janet")

(declare-executable
  :name "csharp-fn-parser"
  :entry "src/main.janet")
