# 定义需要忽略的 C# 关键字（看起来像调用，但并非函数调用）
(def csharp-non-call-words
  ["if" "for" "while" "switch" "catch" "using" "return" "lock" "foreach"
   "typeof" "sizeof" "nameof" "checked" "unchecked" "default"])

(def keyword-set
  (do
    (var t @{})
    (each k csharp-non-call-words
      (put t k true))
    t))

(defn csharp-non-call-word? [k]
  (get keyword-set k))

# 一个简单的去重（保持顺序）
(defn unique-preserve [xs]
  (var seen @{})
  (var out @[])
  (each x xs
    (when (not (get seen x))
      (put seen x true)
      (array/push out x)))
  out)

# PEG 语法：
# - 跳过字符串/注释
# - 匹配形如 Identifier [<...>] ( ... ) 后面不是 "{" 或 "=>" 的片段
# - 仅捕获 "Identifier" 本身（不含限定名、泛型或括号）
(def grammar
  ~{:ws (any :s)
    :ident-core (sequence (choice :a "_") (any (choice :w "_")))

    # 平衡括号 ( ... )，在内部继续捕获嵌套调用
    :parens (sequence
              "("
              (any (choice :skip :call :parens (if-not (set "()") 1)))
              ")")

    # 平衡尖括号 < ... >，用于跳过泛型参数
    :angles (sequence "<" (any (choice :angles :parens (if-not (set "<>") 1))) ">")

    # 字符串/字符字面量与注释（用于跳过干扰）
    :dqstr (sequence "\"" (any (choice "\\\"" (if-not "\"" 1))) "\"")
    :sqstr (sequence "'" (any (choice "\\'" (if-not "'" 1))) "'")
    :line-comment (sequence "//" (any (if-not "\n" 1)))
    :block-comment (sequence "/*" (thru "*/"))
    :skip (choice :dqstr :sqstr :line-comment :block-comment)

    # 花括号 { ... }，用于跳过 record/class/struct 的主体
    :braces (sequence
              "{"
              (any (choice :skip :braces (if-not (set "{}") 1)))
              "}")

    # 修饰符（可重复出现）
    :modifier (choice "public" "private" "protected" "internal"
                      "abstract" "sealed" "partial" "readonly"
                      "ref" "unsafe" "static" "new")
    :modifiers (any (sequence :ws :modifier))

    # 跳过 record 定义： [修饰符] record [class|struct]? 标识符 [<...>] ( ... ) 后面可接 ; 或 {...}
    :record-def (sequence
                  :modifiers
                  :ws
                  "record"
                  :ws
                  (opt (sequence (choice "class" "struct") :ws))
                  :ident-core
                  (opt :angles)
                  :ws
                  :parens
                  :ws
                  (opt ";"))

    # 跳过 class/struct 定义（支持 C# 12 主构造函数）：
    # [修饰符] (class|struct) 标识符 [<...>] [( ... )] [; | { ... }]
    :type-def (sequence
                :modifiers
                :ws
                (choice "class" "struct")
                :ws
                :ident-core
                (opt :angles)
                :ws
                (opt :parens)
                :ws
                (opt ";"))

    # 一个潜在的调用：仅捕获标识符，不捕获链式访问与泛型
    :call (sequence (capture :ident-core)
                    (any :ws)
                    (opt :angles)
                    :ws
                    :parens
                    :ws
                    (not "{")
                    (not "=>"))

    # 主入口：全局扫描文本
    :main (any (choice :skip :record-def :type-def :call 1))})


# 主函数
(defn main [& args]
  (when (< (length args) 1)
    (eprint "错误: 请提供一个 C# 文件的路径。")
    (eprint "用法: csharp-fn-parser <your-file.cs>")
    (os/exit 1))

  (def file-path (get args (- (length args) 1)))
  (def csharp-code (slurp file-path))

  # 执行 PEG 匹配
  (def matches (peg/match grammar csharp-code))

  # 过滤控制结构 / 伪函数 并去重
  (def called-functions
    (->> matches
         (filter |(not (csharp-non-call-word? $)))
         unique-preserve))

  #(print "在文件中找到的被调用函数:")
  (print (string/join called-functions " " ))
)
