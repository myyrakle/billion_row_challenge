# billion row challenge

Rust, Go, Node.js, Python, Java, C#, PHP, C++ 등의 언어로 진행하는 10억개 데이터 처리 챌린지입니다.

1. setup.sh를 실행해서 테스트 데이터를 생성합니다. (17GB 정도의 크기입니다.)
2. 입력 데이터 파일에는 다음과 같이 지역과 측정값 쌍이 개행으로 구분된 채로 들어있습니다. 숫자는 정수 값입니다.

```
Prico;458634
Canada;780790
Central;798387
Athens;799643
Boa Vista;75486
```

3. 해당 파일을 읽어서 지역명을 기준으로 최댓값, 최솟값, 개수, 총합, 평균값을 구하세요.
4. 나눗셈을 할 경우 integer 연산으로 나머지는 버립니다.
5. 지역명을 기준으로 오름차순 정렬을 합니다.
6. 기대하는 출력 형태는 다음과 같습니다. `지역명=최솟값;최댓값;평균값(총합/개수)`

```
Adenarith=1;9999999;5000594(4716533623284/9431947)
Amsterdam=4;9999999;4999133(4715142248923/9431920)
Anápolis=0;9999999;4999550(4718363502090/9437576)
```

## 제한

- 일반적인 상용 컴퓨팅 환경에서 동작해야 합니다. x86(amd64), linux. RAM 32GB
- 하지만 unsafe나 트리키한 흑마술들은 사용 가능합니다.

## Rust

버전은 1.77.0입니다.
1. rust 경로에 추가 프로젝트를 구성합니다. basic을 복사해도 되고, cargo new로 생성해도 됩니다.

## Go

버전은 Go 1.22.2 입니다.
1. go/cmd 경로에 새 폴더를 만듭니다.
2. go/cmd/basic/main.go 파일을 복사한 뒤에 수정해서 최적화합니다.

## Node.js

버전은 v20.12.2입니다.
1. nodejs 경로에 새 파일을 만듭니다.
2. nodejs/basic.js 파일을 복사한 뒤에 수정해서 최적화합니다.

## Python
버전은 Python 3.0.2입니다.
1. ruby 경로에 새 파일을 만듭니다.

## Ruby
버전은 Python 3.10.12입니다.
1. python 경로에 새 파일을 만듭니다.

## Java
버전은 openjdk 11.0.22입니다.
1. java 경로에 새 폴더를 만들고 프로젝트를 구성합니다.

## C++
버전은 G++ 11.4.0입니다.
1. cpp 경로에 새 폴더를 만들고 프로젝트를 구성합니다.

## C#
버전은 dotnet 8.0.103입니다.
1. c# 경로에 새 폴더를 만들고 프로젝트를 구성합니다.
   -  dotnet new로 직접 생성해도 좋고,
   -  basic을 복제해서 사용해도 좋습니다.

## PHP
버전은 8.1.2입니다.
1. php 경로에 새 소스파일을 작성합니다.

## Erlang
버전은 V12.2.1입니다.
1. erlang 경로에 분리된 새 폴더와 소스파일을 작성합니다.

## 현재 결과,,,,,,

| Rank | Lang     | Code                                          | time      |
| ---- | -------- | --------------------------------------------- | --------- |
| 1    | C++      | [libertyrapid1](./cpp/libertyrapid1/main.cpp) | 3272ms    |
| 2    | Go       | [lemon-mint2](./go/cmd/lemon-mint2/main.go)   | 4365ms    |
| 3    | Go       | [lemon-mint1](./go/cmd/lemon-mint1/main.go)   | 6113ms    |
| 4    | Rust     | [whitetac2](./rust/whitetac2/src/main.rs)     | 6234ms    |
| 5    | Go (GCC) | [lemon-mint3](./go/cmd/lemon-mint3/main.go)   | 6772ms    |
| 6    | C#       | [rudty1](./csharp/rudty1/Program.cs)          | 7674ms    |
| 7    | Rust     | [whitetac1](./rust/whitetac1/src/main.rs)     | 8046ms    |
| 8    | Rust     | [Basic](./rust/basic/src/main.rs)             | 128293ms  |
| 9    | Java     | [Basic](./java/basic/Main.java)               | 184825ms  |
| 10   | C++      | [Basic](./cpp/basic/main.cpp)                 | 190039ms  |
| 11   | Go       | [Basic](./go/cmd/basic/main.go)               | 191004ms  |
| 12   | C#       | [Basic](./csharp/basic/Program.cs)            | 215245ms  |
| 13   | PHP      | [Basic](./php/basic.php)                      | 259109ms  |
| 14   | Node.js  | [sunrabbit1](./nodejs/sunrabbit1/index.js)    | 580870ms  |
| 15   | Node.js  | [Basic](./nodejs/basic.js)                    | 921809ms  |
| 16   | Python   | [Basic](./python/basic.py)                    | 936269ms  |
| 17   | Ruby     | [Basic](./ruby/basic.rb)                      | 1271762ms |
| 18   | Erlang   | [Basic](./erlang/basic/main.erl)              | 3488261ms |

## Reference

- https://github.com/gunnarmorling/1brc
- https://benhoyt.com/writings/go-1brc/
