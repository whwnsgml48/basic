def add(a, b):
    return a+b

print(add(1, 2))
print(add('abc', 'def'))
print(add([1, 2, 3], [4, 5, 6]))

a = [12, 'abcde', 4+2j, [3,4,5]]
a.append('add')
print(a)

#대화식 모드로 간단한 파이썬 예제 실행하기
print(4+5)
print('hello world!')

#산술 연산하기
print(4+5)
print(12-32)
print((4+5)*6)
print(4+5*6)
print(9/5)

#간단한 문자열 연산하기
print('Hello')
print('Hi there!')

a = 'My name is '
b = 'Amenda'
print(a + b)

#파이썬 버전 알아보기
import sys
print(sys.version)
print()
print(sys.version_info)

#대화식 모드에서 혹은 다른 모듈에서 모듈파일 실행하기
exec(open('cal.py').read())

