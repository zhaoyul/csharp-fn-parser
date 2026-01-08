# Simple test runner for csharp-fn-parser.
(import ../src/main :as parser)

(defn parse-calls [code]
  (->> (peg/match parser/grammar code)
       (filter |(not (parser/csharp-non-call-word? $)))
       parser/unique-preserve))

(defn assert-eq [label expected actual]
  (when (not (deep= expected actual))
    (error (string label "\nexpected: " expected "\nactual: " actual))))

(var tests @[])

(defn add-test [name f]
  (array/push tests [name f]))

(add-test "basic-calls"
  (fn []
    (def code
      "public void SayHello() {\n
           Console.WriteLine(\"Enter your name: \");\n
           var name = Console.ReadLine();\n
           if (string.IsNullOrEmpty(name)) {\n
             LogWarning(\"Name cannot be empty.\");\n
             return;\n
           }\n
       GreetUser(name);\n}\n")
    (assert-eq "basic-calls"
               @["WriteLine" "ReadLine" "IsNullOrEmpty" "LogWarning" "GreetUser"]
               (parse-calls code))))

(add-test "ignore-keywords"
  (fn []
    (def code
      "if (IsReady()) {\n
         for (var i = 0; i < 3; i++) {\n
           DoWork();\n
         }\n
      }\n
      return Finish();\n")
    (assert-eq "ignore-keywords"
               @["IsReady" "DoWork" "Finish"]
               (parse-calls code))))

(add-test "ignore-strings-and-comments"
  (fn []
    (def code
      "Console.WriteLine(\"FakeCall()\");\n
       // AnotherFakeCall()\n
       /* BlockFakeCall() */\n
      RealCall();\n")
    (assert-eq "ignore-strings-and-comments"
               @["WriteLine" "RealCall"]
               (parse-calls code))))

(add-test "method-definitions"
  (fn []
    (def code
      "void Declared() { }\n
       int ExprBody() => 1;\n
       void Uses() { Declared(); }\n")
    (assert-eq "method-definitions"
               @["Declared"]
               (parse-calls code))))

(add-test "generics-and-qualified"
  (fn []
    (def code
      "List<int>.Add(item);\n
       Foo<Bar>(Baz());\n")
    (assert-eq "generics-and-qualified"
               @["Add" "Foo" "Baz"]
               (parse-calls code))))

(add-test "unique-order"
  (fn []
    (def code
      "Ping(); Ping(); Pong(); Ping();\n")
    (assert-eq "unique-order"
               @["Ping" "Pong"]
               (parse-calls code))))

(add-test "record-and-type-defs"
  (fn []
    (def code
      "public record Person(string Name);\n
       public class Box(int size) { void Use() { Log(size); } }\n
       public struct Pair(int A, int B);\n")
    (assert-eq "record-and-type-defs"
               @["Log"]
               (parse-calls code))))

(defn run-tests []
  (var passed 0)
  (var total (length tests))
  (each [name f] tests
    (try
      (do
        (f)
        (set passed (+ passed 1)))
      ([e]
        (eprint (string "FAIL: " name))
        (eprint (string e))
        (os/exit 1))))
  (print (string "PASS " passed " / " total " tests")))

(run-tests)
