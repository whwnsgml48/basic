#3. 내장 자료형의 기초

#1 수치 자료형
#1-1 정수형 상수
a = 23
b = 0o23
c = 0x23
print(type(a), type(b), type(c))
print(a, b, c)

import sys
print(sys.maxsize)

#1-2 실수형 상수
a = 1.2
b = 3.5e3
c = -0.2e-4
print(type(a), type(b), type(c))
print(a, b, c)
print()

#1-3 롱형 상수
#메모리가 허용하는 한 유효자리수는 무한대
h1 = 12345678912345678901234556970
print(type(h1))
print(h1*h1)
print()


#1-5 수치 자료형의 치환
x = 1
x = 2
#x가 지니는 1의 값이 변경되는 것이 아니라 새로운 객체 2로 레퍼런스를 변경

#1-6 수치 연산과 관련된 내장 함수
print(abs(-3))
print(int(3.141))
print(int(-3.131))
print(float(5))
print(complex(3.4, 5))
print(complex(6))

print(divmod(5, 2))
print()
print(pow(2,3))
print(pow(2.3,3.5))

#1-7 math 모듈의 수치 연산 함수
import math

print(math.pi)
print(math.e)
print(math.sin(1.0))
print(math.sqrt(2))
print()

r = 5.0
a = math.pi

degree = 60.0
rad = math.pi * degree / 180.0
print(math.sin(rad), math.cos(rad), math.tan(rad))

#2 문자열
#2-1 문자열 형식

print('Hello World!')
print("Hello World!")

#여러분 문자열 형식
multiline = """
To be, or not to be
that is the question
"""
print(multiline)

multiline2 = '''
To be, or not to be
that is the question
'''
print(multiline2)

#2-2 인덱싱과 슬라이싱
s = 'Hello world!'
print(s[0])
print(s[1])
print(s[-1])
print(s[-2])

s = 'Hello world!'
print(s[1:3])
print(s[0:5])
print()

s = 'Hello'
print(s[1:])
print(s[:3])
print(s[:])
print()

s = 'abcd'
print(s[::2])
print(s[::-1])

#문자열 자료형은 변경되지 않는다.
s = 'Hello World'
#s[0] = 'h'
#따라서 문자열을 변경하려면 슬라이싱 및 연결 연산을 주로 이용한다.
s = 'h' + s[1:]
print(s)

#2-3문자열 연산
# + : 연결, * : 반복
print('Hello' + '' + 'World')
print('Hello' * 3)
print('-' * 60)

#2-4문자열의 길이
s = 'Hello World'
print(len(s))

#2-5 문자열내 포함 관계 여부
#in, not in: 문자열내 일부 문자열이 포함되어 있는지 파악하는 키워드
s = 'Hello World'
print('World' in s)
print('World' not in s)

