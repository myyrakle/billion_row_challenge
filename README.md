# billion row challenge

Rust, Go, Node.js 등의 언어로 진행하는 10억개 데이터 처리 챌린지입니다.

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

- 일반적인 상용 컴퓨팅 환경에서 동작해야 합니다. x86, linux. RAM 32GB
- 하지만 unsafe나 트리키한 흑마술들은 사용 가능합니다.

## Rust

버전은 1.77.0입니다.
1. rust 경로에 추가 프로젝트를 구성합니다. basic을 복사해도 되고, cargo new로 생성해도 됩니다.

## Go

버전은 Go 1.22.2 입니다.
1. go/cmd 경로에 새 폴더를 만듭니다.
2. go/cmd/basic/main.go 파일을 복사한 뒤에 수정해서 최적화합니다.

## Node.js

버전은 v18.19.0입니다.
1. nodejs 경로에 새 파일을 만듭니다.
2. nodejs/basic.js 파일을 복사한 뒤에 수정해서 최적화합니다.

## Python
버전은 Python 3.10.12입니다.
1. python 경로에 새 파일을 만듭니다.

## 현재 결과,,,,,,

| Rank | Lang    | Sample                                      | time      |
| ---- | ------- | ------------------------------------------- | --------- |
| 1    | Go      | [lemon-mint1](./go/cmd/lemon-mint1/main.go) | 6113ms    |
| 2    | Rust    | [whitetac1](./rust/whitetac1/src/main.rs)   | 8046ms    |
| 3    | Rust    | [Basic](./rust/basic/src/main.rs)           | 128293ms  |
| 4    | Go      | [Basic](./go/cmd/basic/main.go)             | 191004ms  |
| 5    | Python  | [Basic](./python/basic.js)                  | 936269ms  |
| 6    | Node.js | [Basic](./nodejs/basic.js)                  | 1262149ms |

## Reference

- https://github.com/gunnarmorling/1brc
- https://benhoyt.com/writings/go-1brc/
