# CAD 도면 리스트 반자동 생성 리습

반복되는 시트폼/도곽 블록, TABLE 객체, 또는 같은 크기의 도곽선 폴리라인을 기준으로 `SHEET / DWG NO / TITLE / SCALE` 값을 읽어 AutoCAD 도면 리스트를 생성하는 AutoLISP입니다.

## 빠른 사용법

```text
APPLOAD로 cad_toc_auto.lsp 불러오기
TSET 입력
반복되는 기준 폼 1개 선택
SHEET NO, 도면번호, 도면제목, SCALE 값 칸을 작은 사각형 창으로 각각 지정
TLIST 입력
생성 위치를 찍어서 도면 리스트 생성
```

## 명령어

```text
TSET  = 폼 기준 설정
TLIST = 도면 리스트 생성
TSTAT = 현재 폼 설정 확인
TCFG  = 도면번호/축척 패턴과 출력 설정 조정
TADD  = 폼 모양이나 방향이 완전히 다른 종류가 섞인 도면에서만 추가 등록
```

## 도면 열 때마다 자동 로드하기

가장 쉬운 방법은 `cad_toc_auto.lsp` 자체를 AutoCAD Startup Suite에 한 번 등록하는 것입니다.

```text
APPLOAD 입력
Startup Suite 또는 시작하기 모음 선택
Add/추가 클릭
cad_toc_auto.lsp 선택
AutoCAD 재시작 또는 새 도면 열기
TSET/TLIST 명령어가 바로 먹는지 확인
```

이 작업은 한 번만 하면 됩니다. 단, `cad_toc_auto.lsp` 파일 위치를 옮기면 Startup Suite에서 다시 등록해야 합니다.

`hueflow_toc_autoload.lsp`는 보조 자동로드 파일입니다. `cad_toc_auto.lsp`가 AutoCAD Support File Search Path 안에 있을 때 사용할 수 있습니다.

## 중요한 선택 기준

- TSET에서 선택하는 첫 객체는 이미 만들어진 도면 리스트 표가 아니라, 도면마다 반복되는 실제 시트폼/도곽입니다.
- SHEET NO, 도면번호, 도면제목, SCALE 단계에서는 폼 전체를 선택하지 말고 값이 들어있는 칸만 작게 감싸세요.
- 같은 블록 이름의 폼은 크기가 달라도 읽습니다.
- SHEET NO가 있으면 번호 순서로 정렬합니다.
- SHEET NO가 비어 있으면 선택한 폼의 폭과 높이를 기준으로 왼쪽 묶음부터 위치 순서로 정렬합니다.
- 정확한 순서가 꼭 필요한 도면은 SHEET NO 칸에 번호가 들어있는 상태가 가장 안정적입니다.
- 명령창 미리보기는 앞 12개만 보여주고, 실제 생성 표는 인식된 전체 행 수로 만들어집니다.

## AutoCAD 2014에서 안 될 때

- AutoCAD LT 2014는 AutoLISP를 지원하지 않을 수 있습니다. 일반 AutoCAD 2014인지 먼저 확인하세요.
- ZIP 안에서 바로 불러오지 말고 압축을 푼 뒤 APPLOAD 하세요.
- `cad_toc_auto_2014.lsp` 또는 `cad_toc_auto.lsp`를 불러옵니다.
- APPLOAD 후 명령창에 `Loaded cad_toc_auto.lsp` 문구가 떠야 정상입니다.
- 보안 경고나 로드 차단이 뜨면 `SECURELOAD` 값을 `0`으로 바꾼 뒤 다시 APPLOAD 해보세요.
- `TSET` 또는 `TLIST`가 `Unknown command`로 나오면 리습 로드가 실패한 것이므로 APPLOAD 단계부터 다시 확인하세요.
