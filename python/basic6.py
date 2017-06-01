#7. 문자열 정의 및 기초 연산

#1 시퀀스 자료형의 지원 연산

#1-1 시퀀스 자료형이란?
#저장된 각 요소를 정수 index를 이용하여참조가 가능한 자료형
#시퀀스 자료형 : 문자열, 리스트, 튜플

s= 'abcdef'
L = [100, 200, 300]
t = ('tuple', 1, 2)

#시퀀스 자료형이 가지는 공통적인 여산
 #인덱싱
 #슬라이싱
 #확장슬라이싱
 #연결
 #반복
 #멤버쉽테스트
 #길이 정보
 #for ~ in 문

#1-2 인덱싱
s = 'abcdef'
l = [100, 200, 300]
print(s[0])
print(s[1])
print(s[2])
print()
print(l[1])
l[1] = 900
print(l[1])

#print(l[100])

#1-3 슬라이싱
s = 'abcdef'
L = [100, 200, 300]

print(s[1:3])
print(s[1:])
print(s[:])
print(s[-100:100])
print()
print(L[:-1])
print(L[:2])

#1-4 확장 슬라이싱
#L[start:end:step]
s = 'abcd'
print(s[::2])
print(s[::-1])

#1-5 연결하기

s = 'abc' + 'def'
print(s)

L = [1, 2, 3] + [4, 5, 6]
print(L)

#1-6 반복하기
s = 'abc'
print(s*4)

L = [1,2,3]
print(L*2)

#1-7 멤버십 테스트
s = 'abcde'
print('c' in s)

t = (1,2,3,4,5)
print(2 in t)
print(10 in t)
print(10 not in t)

print('ab' in 'abcd')
print('ad' in 'abcd')
print(' ' in 'abcd')
print(' ' in 'abcd ')

# 1-8 길이정보
s='abcde'
l = [1,2,3]
t = (1,2,3,4)
print(len(s))
print(len(l))
print(len(t))

# 1-9 for~in 문
for c in 'abcd':
    print(c)

#2 문자열 정의하기

#2-1 한 줄 문자열
s = ''
str1 = 'Python is great!'
str2 = 'Yes, it is.'
str3 = "It's not like any other languages"
str4 = 'Don\' walk. "Run"'
print(str4)

# \:다음 라인이 현재 라인의 뒤에 이어짐을 나타냄
long_str = "This is a rather long string " \
           "containing back slash and new lin. \nGood!"
print(long_str)

#2-2 여러 줄 문자열
multiline = """ While the rest of the world has been catching on to
the Perl scripting language, the Linux commnunity,
long since past the pleasing shock of Perl's power,
has been catching on to a different scripting animal -- Python."""
print(multiline)
ml = ''' While the rest of the world has been catching on to
the Perl scripting language, the Linux commnunity,
long since past the pleasing shock of Perl's power,
has been catching on to a different scripting animal -- Python.'''
print(ml)

#이스케이프 문자(escape character)
#문자열 내부의 이스케이픔 ㅜㄴ자

print('\\abc\\')
print()
print('abc\tdef\tghi')
print()
print('a\nb\nc')

#2-3 문자열 연산
str1 = 'First String'
str2 = 'Second String'
str3 = str1 + ' ' + str2
print(str3)

print(str1 * 3)
print()

print(str1[2])
print(str1[1:-1])
print(len(str1))
print()
print(str1[0:len(str1)])

#문자열 자료의 경우 변경이 불가능함.
#따라서 슬라이싱을 이용한 연결을 활용해야함.
