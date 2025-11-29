# Pascal Implementation

## 설치

### Linux (Ubuntu/Debian)
```bash
sudo apt-get update
sudo apt-get install fpc
```

### Linux (Arch/Manjaro)
```bash
sudo pacman -S fpc
```

### Linux (Fedora/RHEL)
```bash
sudo dnf install fpc
```

### macOS
```bash
brew install fpc
```

### 설치 확인
```bash
fpc -version
```

## 실행

프로젝트 루트 디렉토리에서:

```bash
./run_pascal.sh
```

또는 직접 컴파일 및 실행:

```bash
# 컴파일
fpc -O3 -XX -CX pascal/basic/main.pas -opascal/basic/basic

# 실행
pascal/basic/basic
```

## 컴파일 옵션

- **`-O3`**: 최고 수준 최적화
- **`-XX`**: Smartlinking 활성화 (사용하지 않는 코드 제거)
- **`-CX`**: Smartlinking 활성화 (더 작은 실행 파일)

더 공격적인 최적화:
```bash
fpc -O4 -XX -CX -Xs -Si pascal/basic/main.pas -opascal/basic/basic
```

추가 옵션:
- **`-O4`**: 더 공격적인 최적화
- **`-Xs`**: Strip symbols (실행 파일 크기 감소)
- **`-Si`**: 인라인 함수 활성화

## 구현 특징

- **한 줄씩 읽기**: `ReadLn`으로 메모리 효율적으로 파일 처리
- **TFPGMap 사용**: Free Pascal의 제네릭 맵으로 도시별 통계 관리
- **Record 타입**: `TStatus` 레코드로 통계 데이터 관리
- **정렬**: 자동 정렬되는 맵 사용
- **네이티브 컴파일**: 기계어로 직접 컴파일되어 빠른 실행

## 성능

Free Pascal Compiler는 네이티브 기계어를 생성하므로 C/C++와 비슷한 성능을 제공합니다.

- 컴파일 시간: 매우 빠름 (1-2초)
- 실행 시간: C++ basic과 유사한 수준 예상
- 메모리 사용: 한 줄씩 읽으므로 효율적

## 예상 실행 시간

- 10억 줄 기준: 약 1-3분 (최적화 빌드)
- 한 줄씩 읽는 방식으로 메모리 사용량 최소화

## Free Pascal 특징

- **크로스 플랫폼**: Windows, Linux, macOS 모두 지원
- **네이티브 컴파일**: 인터프리터가 아닌 컴파일러
- **빠른 컴파일**: GCC보다 훨씬 빠른 컴파일 속도
- **좋은 성능**: 최적화된 네이티브 코드 생성
- **Delphi 호환**: Object Pascal / Delphi 코드와 호환