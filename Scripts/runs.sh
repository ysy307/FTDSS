#!/bin/zsh

# 対象ディレクトリ群
dirs=(
    "/workspaces/FTDSS/Inout/1Domain-Triangle1st"
    "/workspaces/FTDSS/Inout/1Domain-Square1st"
    "/workspaces/FTDSS/Inout/1Domain-Triangle2nd"
    "/workspaces/FTDSS/Inout/1Domain-Square2nd"
)

# 例: pipe 名と距離リスト（適宜編集）
pipes=("PipeA" "PipeB")
distances=("100" "200")

# パスを書き換える対象のファイル（フルパス or 相対パス）
project_path_file="./ProjectPath.dir"

# 各ディレクトリに対して実行
for dirpointer in "${dirs[@]}"; do
      echo "🔧 Processing: $dirpointer"

      # ProjectPath.dir にパスを書き換え
      echo "$dirpointer" > "$project_path_file"

      # ./bin/test を実行
      ./bin/test
done
