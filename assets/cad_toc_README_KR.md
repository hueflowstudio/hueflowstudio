# CAD 도면 리스트 반자동 생성 리습

AutoCAD에서 반복되는 시트폼 블록 또는 TABLE 객체를 기준으로 도면 리스트를 자동 생성하는 AutoLISP입니다.

기존 도면을 전부 속성 블록으로 바꾸지 않아도, 이미 작성된 도면의 시트폼 안에 있는 일반 텍스트나 TABLE 셀 값을 읽어 `SHEET / DWG NO / TITLE / SCALE` 목록을 생성합니다.

## 빠른 사용법

```text
APPLOAD로 cad_toc_auto.lsp 불러오기
→ TOCFORMSET으로 기준 폼 1개 선택
→ 번호/제목/스케일 위치 지정
→ DWG 저장
→ TOCFORMSCAN 실행
→ 도면 리스트 자동 생성
```

```text
[Hueflow CAD 도면 리스트 자동화 리습 사용법]

1. AutoCAD에서 APPLOAD 입력
2. cad_toc_auto.lsp 파일 불러오기
3. 명령어 TOCFORMSET 입력
4. 기준이 되는 시트폼/도곽 블록 또는 TABLE 객체 1개 선택
5. 화면 안내에 따라 SHEET NO, 도면번호, 도면제목, SCALE 위치를 각각 지정
6. DWG 저장
7. 명령어 TOCFORMSCAN 입력
8. 도면 안의 같은 시트폼들을 자동으로 읽어서 도면 리스트 생성

주요 명령어

TOCFORMSET
- 기준 시트폼을 등록하는 명령어
- 한 번만 세팅하면 같은 폼의 도면번호, 제목, 스케일 위치를 DWG 안에 저장함

TOCFORMSTATUS
- 현재 도면에 저장된 폼 세팅이 있는지 확인하는 명령어
- 데스크탑/다른 PC에서 안 될 때 먼저 확인하면 좋음

TOCFORMSCAN
- 등록된 시트폼을 기준으로 전체 도면 리스트를 자동 생성하는 명령어
- 같은 블록/폼 안의 텍스트를 읽어서 표 형태로 정리함

TOCCFG
- 도면번호 패턴, 스케일 인식 등 기본 설정을 조정하는 명령어

추천 사용 흐름

APPLOAD → cad_toc_auto.lsp 불러오기
→ TOCFORMSET으로 기준 폼 1개 세팅
→ DWG 저장
→ TOCFORMSCAN으로 전체 리스트 생성
→ 빠진 항목이나 상세도는 수동 보정

주의사항

- 같은 시트폼/도곽 블록 또는 같은 크기의 TABLE 객체를 반복해서 쓰는 도면일수록 정확도가 높음
- 도면 제목, 도면번호, 스케일이 일반 텍스트 또는 TABLE 셀 값으로 들어있어야 인식 가능
- PC를 옮길 때는 TOCFORMSET 후 DWG를 저장해야 폼 위치 세팅이 같이 넘어감
- 다른 시트에 있는 상세도나 폼 구조가 완전히 다른 도면은 수동 보정 필요
```

## 핵심 기능

- 반복되는 시트폼 블록 또는 TABLE 객체를 기준으로 값 칸 위치를 학습
- 같은 폼 블록 전체를 자동 스캔
- 시트번호, 도면번호, 도면명, 축척을 표로 생성
- AutoCAD Table 또는 기존 목차선 위 TEXT 출력 지원
- `L-000` 같은 예외 항목은 수동 추가 가능

## 리습 파일

`cad_toc_auto.lsp`

## 로드 방법

AutoCAD 명령창에서:

```lisp
(load "C:/Users/kimyj/cad_toc_auto.lsp")
```

또는 `APPLOAD`로 `cad_toc_auto.lsp`를 로드합니다.

## 기본 사용 순서

### 1. 폼 기준 설정

명령어:

```text
TOCFORMSET
```

순서:

```text
1. 반복되는 시트폼 블록 또는 TABLE 객체 하나 선택
2. SHEET NO 값 칸을 박스로 지정
3. DWG NO 값 칸을 박스로 지정
4. TITLE 값 칸을 박스로 지정
5. SCALE 값 칸을 박스로 지정
6. DWG 저장
```

주의: 2~5번은 폼 전체가 아니라 값이 들어가는 칸만 작게 지정합니다.

`TOCFORMSET`으로 잡은 폼 세팅은 현재 DWG 안에 저장됩니다. 다른 PC에서 같은 DWG를 열면 `APPLOAD` 후 `TOCFORMSCAN`으로 바로 사용할 수 있습니다.

저장된 폼 세팅이 있는지 확인하려면:

```text
TOCFORMSTATUS
```

### 2. 리스트 생성

명령어:

```text
TOCFORMSCAN
```

추천 선택:

```text
Create output? → Yes
Output mode → Table
Table insertion point → 빈 곳 클릭
```

## 보조 명령

```text
TOCSEMI
```

선택한 도면 제목란/텍스트 영역을 반자동 인식합니다.

```text
TOCSEMIALL
```

DWG 전체를 스캔하되, 기존 목차 영역을 제외하고 반자동 인식합니다.

```text
TOCCFG
```

도면번호 패턴, 축척 패턴, 출력 기본값 등을 설정합니다.

## 적합한 도면 조건

- 같은 이름의 시트폼 블록 또는 같은 크기의 TABLE 객체를 여러 도면에 반복 사용
- 도면번호, 도면명, 축척이 폼 안의 일정한 칸에 위치
- 값이 일반 TEXT/MTEXT 또는 TABLE 셀 값이어도 가능
- 폼이 스케일되어도 상대 위치 기준으로 추적 가능

## 한계

- 폼 블록 이름이나 TABLE 크기가 여러 종류면 종류별로 `TOCFORMSET`을 다시 설정해야 합니다.
- 표지처럼 다른 폼을 쓰는 항목은 수동 추가가 더 빠를 수 있습니다.
- 도면명 텍스트가 칸 밖에 있거나 여러 객체로 너무 흩어져 있으면 일부 수정이 필요할 수 있습니다.

## 권장 워크플로우

```text
TOCFORMSET
→ DWG 저장
→ TOCFORMSCAN
→ 생성된 Table 확인
→ 누락/예외 항목만 수동 보정
```
