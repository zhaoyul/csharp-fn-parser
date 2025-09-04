# 定义 C# 关键字，用于过滤
(def csharp-keywords
  @{"if" "for" "while" "switch" "catch" "using" "return" "lock" "foreach" "typeof"})

# 定义 PEG 语法
(def grammar
  ~{:main (any (or :potential-call 1))
    :potential-call (sequence :identifier :ws "(")
    :ws (any (set " \t\r\n"))
    :identifier (capture (sequence (range "a-zA-Z_")
                                   (any (range "a-zA-Z0-9_"))))})

# 主函数
(defn main [& args]
  # 检查是否提供了命令行参数
  (if (< (length args) 1)
    (do
      (eprint "错误: 请提供一个 C# 文件的路径。")
      (eprint "用法: ./csharp-fn-parser <your-file.cs>")
      (os/exit 1))
    nil)

  (def file-path (first args))
  (var csharp-code nil)

  # 读取文件内容，并处理可能发生的错误
  (if-let [err (try
                 (set csharp-code (slurp file-path))
                 nil # try block returns nil on success
                 ([_] _))] # catch block returns the error
    (do
      (eprint (string/format "错误: 无法读取文件 '%s': %s" file-path err))
      (os/exit 1)))

  # 使用 PEG 匹配代码
  (def matches (peg/match grammar csharp-code))

  # 过滤掉关键字
  (def called-functions
    (filter |(not (csharp-keywords $))
            matches))

  # 打印结果
  (print "在文件中找到的被调用函数:")
  (each func called-functions
    (print (string/format "- %s" func))))
