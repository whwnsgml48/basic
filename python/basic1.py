#2. 파이썬 언어의 기본문형

#1 파이썬 예약어 및 내장 함수

#1-1 예약어
#파이썬에서 이미 문법적인 용도로 사용되고있기 때문에 변수명 등의 식별자로 사용되면 안되는 단어

#1-2 예약어의 종류
import keyword
print(keyword.kwlist, '\n' ,len(keyword.kwlist))

#1-3 내장함수
#별도의 모듈 추가 없이 기본적으로 제공되는 함수

#abs(x) 수치형 자료 x의 절대값을 반환
print(abs(3))
print(abs(-3))

#max(s) 시퀀스 자료형(문자열, 리스트, 튜플)을 입력받아 그자료가 지닌 원소 중 최대값 반환
print(max(1, 2))
print(max([1,2,3]))
print(max('python'))
print()

#min(s) 시퀀스 자료형(문자열, 리스트, 튜플)을 입력받아 그자료가 지닌 원소 중 최소값 반환
print(min(1,2))
print(min([1,2,3]))
print(min('python'))
print()

#pow(x,y) 수치형 자료형 x,y에 대해 x의 y승을 반환하는 함수
print(pow(2, 4))
print(pow(3, 3))
print(pow(2, -1))
print()

#chr(i)정수 형태의 아스키 코드 값을 입력 받아 그에 해당하는 문자를 반환하는 함수 0~255
print(chr(97))
print(chr(65))
print(chr(48))
print()

#str(object) 임의의 객체 objcet에 대해 해당 객체를 표현하는 문자열을 반환하는 함수
print(str(3))
print(str([1, 2]))
print()

#range([start,]stop[,step])
print(range(10))
print(range(3,10))
print(range(3,10,2))
print()

#type(object) 임의의의 객체 object의 자료형을 반환하는 함수
print(type(-1))
print(type('abc'))
print(type([1,2,3]))
print()

#2 파이썬 식별자와 변수 사용
#2-1 식별자 만드는 법
#2-2 변수명 만들 때 조심할 점
print(str(12345))
str = 'abc'
#print(str(12345))

#2-3변수의 생성 및 사용
a = 1
print(a)

#print(b)
#del b
#print(b)

#3 파이썬 기초 문형
#3-1 주석문

a = 1
b = 3
if(a == 1) and \
        (b==3):
    print('connected lines')

if(a == 1) and (b==3):
    print('connected lines')

#3-3 할당문
a = 1
b = a
#1+3 = a

c, d = 3, 4
print(c,d)
x = y = z = 0
print(x,y,z)

e = 3.5; f = 5.6
print(e, f)

#변수값 swap
e, f = f, e
print(e, f)

#statement는 할당할 수 없다.
#a = (b=c+d)

#3-4 확장 할당문
a = 1
a += 4
print(a)

a=10
a -= 3
print(a)

a = 10
a *= 2+3
print(a)
print()

#3-5객체와 할당
a = [1,2,3]
b = [10,a,20]
c = ['x',a,'y']

print(a)
print(b)
print(c)
print()

a[1] = 100

print(a)
print(b)
print(c)
print()

#4 콘솔 입출력
#4-1 콘솔 입력
#콘솔 : 윈도우 - command, 리눅스/맥 - Terminal
#각 IDE에서는 별도의 콘솔창이 제공됨

#name = input('name?')
#print(name)

#k = int(input('int:'))
#print(k)

#4-2 콘솔 출력
print(4+5, 4-2)
print(1); print(2)

