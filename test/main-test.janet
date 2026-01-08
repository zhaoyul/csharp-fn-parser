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
  (array/push tests @[name f false]))

(defn add-xfail [name f]
  (array/push tests @[name f true]))

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

(add-test "null-conditional-and-invocation"
  (fn []
    (def code
      (string/join
        ["obj?.Do();"
         "handler?.Invoke(Arg());"]
        "\n"))
    (assert-eq "null-conditional-and-invocation"
               @["Do" "Invoke" "Arg"]
               (parse-calls code))))

(add-test "switch-and-with"
  (fn []
    (def code
      (string/join
        ["var x = val switch { _ => Build(Foo()) };"
         "var y = person with { Name = GetName() };"]
        "\n"))
    (assert-eq "switch-and-with"
               @["Build" "Foo" "GetName"]
               (parse-calls code))))

(add-test "tuple-and-named-args"
  (fn []
    (def code
      (string/join
        ["Log((Foo(), Bar()), count: Count());"]
        "\n"))
    (assert-eq "tuple-and-named-args"
               @["Log" "Foo" "Bar" "Count"]
               (parse-calls code))))

(add-test "using-and-lock"
  (fn []
    (def code
      (string/join
        ["using (Create()) {"
         "  lock (GetLock()) { Run(); }"
         "}"]
        "\n"))
    (assert-eq "using-and-lock"
               @["Create" "GetLock" "Run"]
               (parse-calls code))))

(add-test "local-function-and-lambda"
  (fn []
    (def code
      (string/join
        ["int Local(int x) => x;"
         "Func<int, int> f = y => Transform(y);"]
        "\n"))
    (assert-eq "local-function-and-lambda"
               @["Transform"]
               (parse-calls code))))

(add-test "pattern-matching"
  (fn []
    (def code
      (string/join
        ["var ok = obj is { Prop: Foo() };"
         "var res = val switch { 1 => A(), _ => B() };"]
        "\n"))
    (assert-eq "pattern-matching"
               @["Foo" "A" "B"]
               (parse-calls code))))

(add-test "primary-constructors"
  (fn []
    (def code
      (string/join
        ["public class C(int x) {"
         "  public int P => Get();"
         "  public void M() { Use(x); }"
         "}"]
        "\n"))
    (assert-eq "primary-constructors"
               @["Get" "Use"]
               (parse-calls code))))

(add-test "unsafe-fixed"
  (fn []
    (def code
      (string/join
        ["unsafe {"
         "  fixed (int* p = GetPtr()) {"
         "    UsePtr(p);"
         "  }"
         "}"]
        "\n"))
    (assert-eq "unsafe-fixed"
               @["GetPtr" "UsePtr"]
               (parse-calls code))))

(add-test "conditional-access-indexer"
  (fn []
    (def code
      (string/join
        ["obj?[GetIndex()].Do(Call());"]
        "\n"))
    (assert-eq "conditional-access-indexer"
               @["GetIndex" "Do" "Call"]
               (parse-calls code))))

(add-test "collection-initializer-stackalloc"
  (fn []
    (def code
      (string/join
        ["var xs = new List<int> { Make(), 1 };"
         "Span<int> s = stackalloc int[] { Init() };"]
        "\n"))
    (assert-eq "collection-initializer-stackalloc"
               @["Make" "Init"]
               (parse-calls code))))

(add-test "function-pointer-call"
  (fn []
    (def code
      (string/join
        ["delegate*<int, void> fp = &Target;"
         "fp(Arg());"]
        "\n"))
    (assert-eq "function-pointer-call"
               @["fp" "Arg"]
               (parse-calls code))))

(add-test "checked-unchecked"
  (fn []
    (def code
      (string/join
        ["var x = checked(Foo());"
         "var y = unchecked(Compute());"]
        "\n"))
    (assert-eq "checked-unchecked"
               @["Foo" "Compute"]
               (parse-calls code))))

(add-test "raw-string-literals"
  (fn []
    (def code
      (string/join
        ["var s = \"\"\""
         "FakeCall()"
         "\"\"\";"
         "Real();"]
        "\n"))
    (assert-eq "raw-string-literals"
               @["Real"]
               (parse-calls code))))

(add-test "collection-expressions"
  (fn []
    (def code
      (string/join
        ["var xs = [1, 2, Make(), ..GetRange()];"]
        "\n"))
    (assert-eq "collection-expressions"
               @["Make" "GetRange"]
               (parse-calls code))))

(add-test "indexer-and-array-size"
  (fn []
    (def code
      (string/join
        ["var x = arr[GetIndex()];"
         "Span<int> s = stackalloc int[GetSize()];"]
        "\n"))
    (assert-eq "indexer-and-array-size"
               @["GetIndex" "GetSize"]
               (parse-calls code))))

(add-test "attribute-arguments"
  (fn []
    (def code
      (string/join
        ["[Obsolete(\"x\")]"
         "void M() { }"
         "[MyAttr(typeof(Foo), nameof(Bar))]"
         "void N() { }"]
        "\n"))
    (assert-eq "attribute-arguments"
               @[]
               (parse-calls code))))

(add-test "verbatim-strings"
  (fn []
    (def code
      (string/join
        ["var s = @\"FakeCall()\";"
         "Real();"]
        "\n"))
    (assert-eq "verbatim-strings"
               @["Real"]
               (parse-calls code))))

(add-test "interpolated-strings"
  (fn []
    (def code
      (string/join
        ["var s = $\"Hello {Name()} {Get()}\";"
         "Real();"]
        "\n"))
    (assert-eq "interpolated-strings"
               @["Real"]
               (parse-calls code))))

(add-test "nested-calls"
  (fn []
    (def code
      (string/join
        ["Outer(Inner(Deep()), Another(Call1(), Call2(Inner2())));"
         "Final();"]
        "\n"))
    (assert-eq "nested-calls"
               @["Outer" "Inner" "Deep" "Another" "Call1" "Call2" "Inner2" "Final"]
               (parse-calls code))))

(add-test "object-initializer"
  (fn []
    (def code
      (string/join
        ["var x = new Foo();"
         "var y = new Bar() { X = Baz() };"]
        "\n"))
    (assert-eq "object-initializer"
               @["Foo" "Baz"]
               (parse-calls code))))

(add-test "complex-generics"
  (fn []
    (def code
      (string/join
        ["Namespace.Type<Dictionary<string, List<int>>>.GetOrAdd(key, Create<int>(Factory()));"]
        "\n"))
    (assert-eq "complex-generics"
               @["GetOrAdd" "Create" "Factory"]
               (parse-calls code))))

(defn run-tests []
  (var passed 0)
  (var xfailed 0)
  (var xpassed 0)
  (var total (length tests))
  (each [name f xfail?] tests
    (try
      (do
        (f)
        (if xfail?
          (do
            (eprint (string "XPASS: " name))
            (set xpassed (+ xpassed 1)))
          (set passed (+ passed 1))))
      ([e]
        (if xfail?
          (do
            (eprint (string "XFAIL: " name))
            (eprint (string e))
            (set xfailed (+ xfailed 1)))
          (do
            (eprint (string "FAIL: " name))
            (eprint (string e))
            (os/exit 1))))))
  (print (string "PASS " passed " / " total " tests; XFAIL " xfailed "; XPASS " xpassed)))

(run-tests)
