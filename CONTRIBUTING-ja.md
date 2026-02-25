# 数値計算アプリケーション コーディング規約

この規約は，Fortran・C・C++を併用する本プロジェクトにおけるコードの品質，保守性，可読性を維持・向上させることを目的とします．

---

## 1. ファイルとディレクトリ 📂

* **ファイル拡張子**: ソースファイルの種類に応じて，以下の拡張子に統一します．
  * **Fortran**: `.F90`
  * **C**: `.c`
  * **C++**: `.cpp`
  * **ヘッダー**: `.h`

* **Fortranモジュール名**: モジュール名は，`src/` ディレクトリからの相対パスに基づいて命名します．ディレクトリの区切り文字 (`/`) はアンダースコア (`_`) に置き換えます．
  * **例**: ファイル `src/core/matrix_utils.F90` には `module core_matrix_utils` を定義します．
* 例外として，moduleをまとめるファイル`src/core/core.F90`のモジュール名には `module module_core` と定義します．

### **Fortranモジュールのuse順序**

* intrinsicなmoduleの読み込みを最初に行います．例：`iso_fortran_env`，`iso_c_binding`
* 次に外部ライブラリを読み込みます．例：`omp_lib`，`MKL`，`stdlib_*`
* プロジェクト内のモジュールを読み込みます．
* 今のファイルが帰属しているフォルダ以外からのmoduleを読み込みます．そのあとに自分の帰属しているフォルダから使うモジュールを読み込みます．
  * 例：今の編集ファイル `src/domain/element_factory.F90` では，別のフォルダ以下で実装している `module_core` を先に読み込んで，その後に `domain/*` でつかうmoduleを読み込みます．

---

## 2. 命名規則 📜

### **変数・定数**

* 原則として**スネークケース (`snake_case`)** を使用します．
  * 例 (Fortran/C): `local_variable`, `max_iteration`

* **C++** のソースコードに限り，**キャメルケース (`camelCase`)** の使用を許可します．
  * 例 (C++): `localPosition`, `temperatureData`

### **関数・サブルーチン**

* 処理内容が明確にわかるように **`動詞_目的語`** の形式を推奨します．
  * 例: `calculate_force()`, `initialize_state()`

### **Fortran 構造体**

* 構造体の名前は `type_` から始める．同じ機能を持つ構造体は抽象型を継承して作成する．抽象型および抽象インターフェースの名前は `abst_` から始める．
* 構造体の初期化は，動的宣言の場合には関数，静的宣言している場合にはサブルーチンで初期化を行う．
  * 動的宣言している場合の関数名は `construct_{構造体名}` とする．
  * 静的宣言している場合のサブルーチン名は `initialize_{構造体名}` とする．

* 抽象型ポリモフィズム構造体はその元の抽象型名を使って `holder_{抽象型名}s` と定義します．複数形になることに注意してください．

### **C++クラス・構造体**

* 単語の先頭を大文字にする**パスカルケース (`PascalCase`)** を使用します．
* 例: `class MatrixSolver`, `struct ParticleData`

---

## 3. 書式・スタイル ✍️

* **1行の長さ**: 1行の最大文字数は **132文字** とします．これを超える場合は適切に改行してください．
* **インデント**: 半角スペース4つによるインデントを推奨します．タブ文字の使用は避けてください．

### 変数属性の並び順

```fortran
[ real | integer | logical | character | type(type_name) | class(class_name) ], &
    [ public | private | protected ], &
    [ parameter | intent(in | inout) ], &
    [ value | optional ], &
    [ public | private | protected ], &
    [ allocatable | pointer ], &
    [ contiguous ], &
    [ save | target ], &
    [ dimension(:) ], &
    [ volatile ] &
:: variable_name

```

ここで `dimension(:)` は配列ポインタを指し示すときにのみ使い，動的割当配列には用いません．動的割当配列の場合は，変数名の後ろに `variable_name(:)` とかっこを付け，合わせて `allocatable` を指定してください．
`intent(out)` の使用は禁止します．

### 型束縛手続きの属性の並び順

```fortran
    procedure, &
        [generic | operator | assignment | final], &
        public | private, &
        pass | nopass, &
        deferred | non_overridable &
    :: binding_name [=> procedure_name]

```

### 暗黙の型使用について

すべてのFortranコードにおいて，暗黙の型定義は禁止します．すべてのモジュール，サブルーチン，関数の頭には必ず `implicit none` を記述してください．

---

## 4. 言語機能の利用方針 🛠️

* **言語標準**:
* **古い機能の禁止**: Fortranの `goto`, `common` ブロックなど，レガシーな機能の使用は禁止します．
* **使用する標準**: 安定性と機能性のバランスを考慮し，**Fortran 2008** および **C++17** をターゲット標準とします．これより新しすぎる機能の採用は，チームでの合意がある場合に限定します．

* **言語間連携**:
  * FortranとC/C++間の連携は，**`iso_c_binding` モジュールを介した方法に限定**します．コンパイラ独自拡張など，これ以外の連携方法は禁止です．
  * 連携インターフェースは特定のラッパーモジュールやファイルに集約し，可読性と管理性を高めることを強く推奨します．

---

## 5. コメント 💬

* コードの可読性を高めるためのコメント追加は推奨されますが，**規約としてコメントの記述スタイルや量を強制するものではありません**．開発者個人の判断に委ねます．
* ただし，複雑なアルゴリズムや，一読して意図が分かりにくい箇所には，後から読む人のために「なぜこの処理が必要か (Why)」を簡潔に残すことが望ましいです．

---

## 6. Fortran Type-Bound Procedure 設計指針

### 6.1. 目的

* Fortran OOPにおけるメソッドの**可読性・保守性**を向上させる．
* メソッドの役割に応じた分類と記述順序を明確化し，大規模なクラスでも目的の処理を即座に見つけられるようにする．
* 状態変更（副作用）を伴う操作と，問い合わせ（副作用なし）を物理的・論理的に区別する．

---

### 6.2. メソッドの分類と定義

| カテゴリ | 役割 | 引数 (`self`) | 戻り値 | 命名例 |
| --- | --- | --- | --- | --- |
| **Lifecycle** | インスタンスの生成・破棄・再初期化 | `intent(inout)` | なし | `initialize`, `destroy`, `reset` |
| **Mutator** | 内部状態（成分）の変更 | `intent(inout)` | なし | `set_*`, `increment_*`, `update_*` |
| **Algorithm** | 主要な計算処理・演算の実行 | `intent(inout/in)` | 任意 | `compute_*`, `check_*`, `solve` |
| **Inquiry** | 内部状態の論理判定（副作用なし） | `intent(in)` | `logical` | `is_*`, `has_*`, `should_*` |
| **Getter** | 内部データの参照（副作用なし） | `intent(in)` | 値 または ポインタ | `get_*` |
| **Meta / Utility** | デバッグ・情報取得・文字列整形 | `intent(in)` | 任意 | `display`, `to_string` |
| **Operator** | 演算子のオーバーロード | `intent(inout/in)` | 任意 | `operator(+)`, `assignment(=)` |

---

### 6.3. 並び順とセクション区切り

`type` 定義内の `contains` 以降に以下のセクション区切りを使用してください．引数名は `self` で固定します．

```fortran
contains
    ! ---- Lifecycle ----
    ! initialize, destroy, reset, etc.

    ! ---- Mutator ----
    ! set_XXX, increment_XXX, update_XXX, etc.

    ! ---- Algorithm / Operation ----
    ! compute_XXX, check_XXX, etc.

    ! ---- Inquiry ----
    ! is_XXX, has_XXX, should_XXX, etc.

    ! ---- Getter ----
    ! get_XXX, etc. (Returns value or pointer)

    ! ---- Meta / Utility ----
    ! display, to_string, etc.

    ! ---- Operator ----


```

### 6.4. 設計上の注意点

* **引数名の固定**:
Passed-object dummy argument（自身を指す引数）の名前は `self` で統一してください．
* **Inquiry / Getter の副作用禁止**:
これらは原則として内部状態を変更しない `pure` 関数として実装することを推奨します．
* **Getter のポインタ返し**:
大規模な配列や動的な構造を返す際，コピーコストを避けるために `pointer` 属性を持つ関数として実装する場合も，このセクションに含めます．
* **Algorithm の分類**:
計算の進捗確認（`check_convergence` など）は，内部状態を更新する場合があるため Algorithm に分類します．論理値を返すだけであれば Inquiry への分類も検討してください．

---

### 6.5. 実装フロー

1. **Lifecycle** から順に定義を並べる．
2. 各メソッドが「副作用あり（Mutator/Algorithm）」か「副作用なし（Inquiry/Getter）」かを明確にする．
3. セクションコメントを活用し，コードの凝集度を高める．
4. 配列操作が含まれる場合は `elemental` 属性の付与を検討する．
