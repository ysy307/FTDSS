## Software environment
* Fortran 90 and later
* Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2024.1.0 Build 20240308
* Lis version 2.1.3
* GNU Make 3.81

## Operation System
### Laptop environment
エディション	Windows 11 Home <br>
バージョン	23H2 <br>
インストール日	2023/09/11 <br>
OS ビルド	22631.3958 <br>
エクスペリエンス	Windows Feature Experience Pack 1000.22700.1026.0

### Desktop environment
#### Host OS
エディション	Windows 11 Pro <br>
バージョン	23H2 <br>
インストール日	2024/05/24 <br>
OS ビルド	22631.3958 <br>
エクスペリエンス	Windows Feature Experience Pack 1000.22700.1026.0

#### Windows subsytem for linux (WSL2)
Ubuntu 24.04.1 LTS <br>
Red Hat Enterprise Linux release 9.4 (Plow)




## Execution environment
### Laptop environment
* CPU Intel Core i5 10210U
* MB FUJITSU CLIENT COMPUTING LIMITED FJNBB6A
* Memory DDR4-3200 4GB×2

### Desktop environment
* CPU Intel Core i9 14900K
* MB Z790 Steel Legend WiFi
* Memory DDR5-5600 32GB×4
* GPU NVIDIA GeForce RTX 4060Ti

# Input
## Basic.in
### 1行目
- 要素数
- 節点数
- 要素頂点数
- 要素次元
- 境界条件節点数
- 境界条件種類
### 2行目
- 使うソルバー（現状ほぼ使っていない）
  - __0__ MKL LU refactorization __1__ CSR
- 使う潜熱処理手法
  - __1__ TRM __2__ GCC __3__ Po0wer → __10__ TRM0 __11__ TRM1 __20__ GCC __30__ Power
- 地下水流動
  - __0__ なし　__1__ あり
- 時間離散化(使ってない)
  - __0__ 陽解法　__1__ 陰解法　__2__ クランクニコルソン法 → 値　$` (0\leq x \leq 1) `$
- 時間経過を標準出力に出すか
  - __0__ 出さない　__1__ 出す
- 任意の時間刻みでのファイル出力をするか
  - __0__ しない　__1__ する

### 3行目