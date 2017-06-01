#4. 리스트, 튜플, 사전 및 내장 자료형 특성

#1. 리스트, 튜플, 사전

#1-1 리스트의 정의와 리스트 기본 연산
#리스트 : 임의의 객체를 순차적으로 저장하는 집합적 자료형
#문자열이 지닌 대부분의 연산들은 리스트도 지원
L= [1,2,3]

print(type(L))
print()

print(len(L))
print()

print(L[1])
print(L[-1])
print(L[1:3])
print()

print(L+L)
print(L*3)

#리스트는 변경가능
l1 = [4,5,6]
l1[0] = 10
print(l1)

l1 = [1,2,3]
l1 = [4,5,6]
print(l1)

#1-2 range() 함수를 통한 인덱스 리스트 생성
#range(k) : 0부터 k-1까지의 숫자의 리스트를 생성함.
L = range(10)
print(L)
print(L[::2])
print(L[::-1])
print(4 in L)

#1-3 튜플의 정의와 기본 연산
t = (1,2,3)
print(len(t))
print()
print(t[0])
print(t[-1])
print(t[0:2])
print(t[::2])
print()
print(t + t + t)
print(t * 3)
print()
print(3 in t)
print()

#1-4 튜플의 상수적 성격
#튜플은 내용 변경 불가
t = (1,2,3)
#t[0] = 100

L = [1,2,3]
L[0] = 100
print(L)

#1-5 사전의 정의와 기본 사용법
#정수형 인덱스가 아닌 키를 이용하여 값을 저장하는 자료구조
#매핑 함수와 비슷한 역할을 함.

d = {'one': 'hana','two': 'dul','three': 'set'}
print(d['one'])

#새항목삽입
d['four'] = 'net'
print(d)
print()
d['one'] = 1 # 기존 항목 값 변경
print(d)
print()
print('one' in d)# 멤버십 테스트

#2 내장 자료형의 정리와 객체 신원 파악
#2-1 내장 자료형의 특성 정리
#2-2내장 자료형 알아보기
print(type(3))
print(type(3.3))
print(type('abc'))

print(type([]))
print(type(()))
print(type({}))

#자료형의 비교
a = 0
L = [1,2,3]
print(type(a) == type(0))
print(type(L) == type([]))
print(type(L[0]) == type(0))

print(type(None))#None 객체, 아무 값도 없다(혹은 아니다)를 나타내는 객체
print()
a = None
print(a)
print(type(a))

#2-3 객체의 신원 식별하기
#id() : 객체의 식별자를 반환한다.
a = 500
b = a
print(id(a))
print(id(b))
print()
x = 1
y = 1
print(id(x))
print(id(y))

#is 키워드 : 두객체의 식별자가 동일한지 테스트 한다.
c = [1,2,3]
d = [1,2,3]
print(c is d)

a = 500
b = a
print(a is b)

x = 1
y = 1
print(x is y)

e = f = [4,5,6]
print(e is f)

# ==연산자 : 두객체의 값이 동일한지 테스트한다.
c = [1,2,3]
d = [1,2,3]
c == d
