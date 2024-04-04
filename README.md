# billion row challenge

Rust, Go, Node.js 등의 언어로 진행하는 10억개 데이터 처리 챌린지입니다.

1. setup.sh를 실행해서 테스트 데이터를 생성합니다.
2. 입력 데이터 파일에는 다음과 같이 지역과 측정값 쌍이 개행으로 구분된 채로 들어있습니다.
```
Montreal;19733015
Curitiba;46857823
Recife;39411216
London;8943412
Paris;2668923
Amsterdam;17039922
```
3. 해당 파일을 읽어서 지역명을 기준으로 최댓값, 최솟값, 개수, 총합, 평균값을 구하세요.
4. 지역명을 기준으로 오름차순 정렬을 합니다.
5. 기대하는 출력 형태는 다음과 같습니다. `지역명=최솟값;최댓값;평균값(총합/개수)` 
```
Adenarith=4;49999998;24999275(235964936500215/9438871)
Amsterdam=4;49999996;24998326(235804987418088/9432831)
Anápolis=0;49999995;25001294(235938792106799/9437063)
```

## 제한
- 해당 언어 안에서 모든 것이 완료되어야 합니다. FFI 같은 것은 반칙입니다.
- 일반적인 상용 컴퓨팅 환경에서 동작해야 합니다. x86, linux. RAM 32GB
- 하지만 unsafe나 트리키한 흑마술들은 사용 가능합니다.

## Rust
1. rust/src 경로에 basic 파일을 복사해서 새 파일을 만듭니다.
2. rust/Cargo.toml에 다음과 같이 실행 경로를 추가합니다.
```
[[bin]]
name = "파일명"
path = "./src/파일명.rs"
```
3. 다음과 같이 실행합니다.
```
cargo run --release --manifest-path ./rust/Cargo.toml --bin 파일명
```

## Go
1. go/cmd 경로에 새 폴더를 만듭니다. 
2. go/cmd/basic/main.go 파일을 복사한 뒤에 수정합니다.

## Node.js
1. nodejs 경로에 새 파일을 만듭니다.
2. nodejs/basic.js 파일을 복사한 뒤에 수정합니다.

## 현재 결과
|Lang|Sample|time|
|---|---|---|
|Go|[BaseLine](./go/cmd/baseline/main.go)|32561ms|
|Rust|[Basic](./rust/src/basic.rs)|144653ms|
|Go|[Basic](./go/cmd/basic/main.go)|213713ms|
|Node.js|[Basic](./nodejs/basic.js)|1021432ms|

## Reference 
- https://github.com/gunnarmorling/1brc
- https://benhoyt.com/writings/go-1brc/
