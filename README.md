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

- 일반적인 상용 컴퓨팅 환경에서 동작해야 합니다. amd64, linux. RAM 64GB, 멀티코어 (8코어)
- 하지만 unsafe나 트리키한 흑마술들은 사용 가능합니다.
- 제출하는 폴더나 소스파일의 이름은 식별 가능하게 닉네임1 or 닉네임-1 같은 형태로 맞춰주세요. 도전 횟수가 느는 만큼 뒤에 붙은 숫자를 증가시켜주시면 됩니다.

## Rust

버전은 1.80.1입니다.
1. rust 경로에 추가 프로젝트를 구성합니다. basic을 복사해도 되고, cargo new로 생성해도 됩니다.

## Go

버전은 Go 1.23.2 입니다.
1. go/cmd 경로에 새 폴더를 만듭니다.
2. go/cmd/basic/main.go 파일을 복사한 뒤에 수정해서 최적화합니다.

## Node.js

버전은 v22.9.0입니다.
1. nodejs 경로에 새 파일을 만듭니다.
2. nodejs/basic.js 파일을 복사한 뒤에 수정해서 최적화합니다.

## Python
버전은 Python 3.12.3입니다.
1. python 경로에 새 파일을 만듭니다.

## Ruby
버전은 Ruby 3.2.3 입니다.
1. ruby 경로에 새 파일을 만듭니다.

## Java
버전은 openjdk 21.0.4입니다.
1. java 경로에 새 폴더를 만들고 프로젝트를 구성합니다.

## C++
버전은 G++ 13.2.0/Clang 18.1.3입니다.
1. cpp 경로에 새 폴더를 만들고 프로젝트를 구성합니다.

## C#
버전은 dotnet 8.0.110입니다.
1. c# 경로에 새 폴더를 만들고 프로젝트를 구성합니다.
   -  dotnet new로 직접 생성해도 좋고,
   -  basic을 복제해서 사용해도 좋습니다.

## PHP
버전은 8.2.2입니다.
1. php 경로에 새 소스파일을 작성합니다.

## Erlang
버전은 V13.2.2.5입니다.
1. erlang 경로에 분리된 새 폴더와 소스파일을 작성합니다.

## Zig
버전은 0.13.0입니다.
1. zig 경로에 새 파일 혹은 새 폴더를 만들고 소스코드를 작성합니다.

## 현재 결과

| Rank | Lang     | Code                                          | time      |
| ---- | -------- | --------------------------------------------- | --------- |
| 1    | C++      | [libertyrapid](./cpp/libertyrapid1/main.cpp) | 903 ms    |
| 2    | Rust     | [s576air](./rust/s576air-1/src/main.rs)     | 1304ms    |
| 3    | Go       | [lemon-mint(v2)](./go/cmd/lemon-mint2/main.go)   | 1566ms    |
| 4    | Go       | [lemon-mint(v1)](./go/cmd/lemon-mint1/main.go)   | 1578ms    |
| 5    | Go (GCC) | [lemon-mint(v3)](./go/cmd/lemon-mint3/main.go)   | 1646ms    |
| 6    | Rust     | [whitetac(v2)](./rust/whitetac2/src/main.rs)     | 1736ms   |
| 7    | Rust     | [whitetac(v1)](./rust/whitetac1/src/main.rs)     | 1986ms    |
| 8    | C#       | [rudty](./csharp/rudty1/Program.cs)          | 2049ms    |
| 9    | Node.js  | [sunrabbit(v2)](./nodejs/sunrabbit2/index.js)    | 20958ms    |
| 10   | Rust     | [Basic](./rust/basic/src/main.rs)             | 75072ms    |
| 11   | Go       | [Basic](./go/cmd/basic/main.go)               | 78757ms    |
| 12   | Java (ParallelGC)     | [Basic](./java/basic/Main.java)               | 85782ms    |
| 13   | C++ (GCC)     | [Basic](./cpp/basic/main.cpp)                 | 86765ms   |
| 14   | C++ (Clang)      | [Basic](./cpp/basic/main.cpp)                 | 93906ms   |
| 15   | Java (G1GC)     | [Basic](./java/basic/Main.java)               | 94091ms    |
| 16   | Java (ZGC)     | [Basic](./java/basic/Main.java)               | 95413ms    |
| 17   | C#       | [Basic](./csharp/basic/Program.cs)            | 125583ms    |
| 18   | PHP      | [Basic](./php/basic.php)                      | 127756ms    |
| 19   | Node.js  | [sunrabbit(v1)](./nodejs/sunrabbit1/index.js)    | 222781ms    |
| 20   | Node.js  | [prravda1](./nodejs/prravda1/index.js)        | 272899ms    |
| 21   | Node.js  | [Basic](./nodejs/basic.js)                    | 353561ms    |
| 22   | Python   | [Basic](./python/basic.py)                    | 508417ms    |
| 23   | Ruby     | [Basic](./ruby/basic.rb)                      | 604528ms    |
| 24   | Zig      | [Basic](./zig/basic.zig)                      | 1641754ms    |
| 25   | Erlang   | [Basic](./erlang/basic/main.erl)              | 1957916ms    |

## Reference

- https://github.com/gunnarmorling/1brc
- https://benhoyt.com/writings/go-1brc/

## 지난결과
- [시즌 1](./README.v1.md)
