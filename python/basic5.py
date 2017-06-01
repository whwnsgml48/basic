#1. 파이썬 제어문
#1-1 들여쓰기와 제어문
#파이썬은 들여쓰기를 강제하여 코드의 가독성을 높인다.
#가장 바깥쪽의 코드는 반드시 1열에 시작한다.
#블록 내부에 있는 statement들은 동일한 열에 위치해야 한다.
#블록의 끝은 들여쓰기가 끝나는 부분으로 간주된다.
#python dpsms {,},begin, end등의 키워드가 존재하지 않는다.
#들여쓰기를 할 때 에는 탭과 공백을 섞어쓰지 않는다.

a = 1
# a = 1

if a > 1:
    print('big')
#        print('really')
#1-2 if문
score = 90
if score >= 90:
    print('Congratulations!!')
a = 10
if a>5:
    print('Big')
else:
    print('Small')

a = 10
if a>5: print('Big')
else : print('Small')

n = -2
if n > 0:
    print('Positive')
elif n < 0:
    print ('Negative')
else:
    print ('Zero')

order = 'spagetti'

if order == 'spam':
    price = 500
elif order == 'ham':
    price = 700
elif order == 'egg':
    price = 300
elif order == 'spagetti' :
    price = 900

print(price)

order = 'spagetti'
menu = {'spam':500, 'ham':700, 'egg':300, 'spagetti':900}
price = menu[order]
print(price)

#1-3for문
a = ['cat', 'cow', 'tiger']
for x in a:
    print(len(x), x)

for x in [1,2,3]:
    print(x,)

print(range(10))
for x in range(10):
    print(x,)

sum = 0
for x in range(1,11):
    sum += x
print(sum)

prod = 1
for x in range(1,11):
    prod *= x

print(prod)

#enumerate() 내장 함수: 컨테이너 객체가 지닌 각 요소값뿐만 아니라 인덱스 값도 함께 반환
l = ['cat', 'dog', 'bird', 'pig']
for k, animal in enumerate(l):
    print(k, animal)
print()

t = ('cat', 'dog', 'bird', 'pig')
for k, animal in enumerate(t):
    print(k, animal)

d = {
    'c': 'cat', 'd':'dog','b':'bird','p':'pig'
}
for k, key in enumerate(d):
    print(k, key, d[key])

#break : 루프를 빠져나감
for x in range(10):
    if x > 3:
        break
    print(x)
print('done\n')

#continue : 루프불록 내의 continue 이후 부분은 수행하지 않고 루프의 시작부분으로 이동한다.
for x in range(10):
    if x < 8:
        continue
    print(x)
print('done\n')

#else : 루프가 break에 의한 중단없이 정상적으로 모두 수행되면 else블록이 수행된다.
for x in range(10):
    print(x)
else:
    print('else block')
print('done\n')

#break에 의하여 루프를 빠져나가면 else 블록도 수행되지 않는다.
for x in range(10):
    break
    print(x,)
else:
    print('else block')
print('done\n')

for x in range(2,4):
    for y in range(2, 10):
        print(x, ' * ', y, ' = ', x*y)
    print()

#1-4 while문
#while 조건식이 만족하는 동안 while 블록내의 statements 들을 반복 수행한다.
count = 1
while count < 11:
    print(count)
    count += 1

sum = 0
a = 0
while a < 10:
    a += 1
    sum += a
print(sum)

x = 0
while x < 10:
    print(x,)
    x = x + 1
else :
    print('else block')
print('done\n')

#2 파이썬 함수 기초
#2-1 함수의 장점 및 함수 사용법
#함수의 장점
#함수는 반복적인 코드를 없애 주어 코드의 길이를 짧게 만들어줌
#코드의 유지보수를 쉽게 만들어줌

def add(a, b):
    return a+b

print(add(3,4))

print(add([1,2,3], [4,5,6]))

c = add(10, 30)
print(c)

#함수 이름에 저장된 레퍼런스를 다른 변수에 할당하여 그 변수를 이용한 함수 호출 가능
f = add
print(f(4, 5))

print(f)

print(f is add)
print()

#함수의 몸체에는 최소한 한개 이상의 statement가 존재해야함.
#그러므로, 아무런 내용이 없는 몸체를 지닌 함수를 만들 때에는 pass라는 키워드를 적어줘야함.

def simple():
    pass

print(simple())

def add(a, b):
    return a + b

def myabs(x):
    if x<0:
        x = -x
    return x

def abbabs(a, b):
    c = add(a, b)
    return myabs(c)

print(abbabs(-5,-7))

#인자의 이름과 함께 인자 값을 넘겨줄 수 있다.
def minus(a,b):
    return a-b
print(minus(a=12, b=20))
print(minus(b=20, a=12))

def incr(x, y=1):
    return x + y

#인자의 디폴트 값 설정 가능
print(incr(5))
print(incr(5,10))

#두 개 이상의 값을 동시 바환 가능
def calc(x,y):
    return x+y, x-y, x*y, x/y

print(calc(10,2))

#2-2 함수 호출시 동적인 자료형 결정
#파이썬에서는 모든 객체는 동적으로 실행시간에 그타입이 결정됨
 #함수 인자는 함수가호출되는 순간 해당 인자에 전달되는 객체에 따라 그타입이 결정
 #함수 몸체 내에서 사용되는 여러 가지 연산자들은 함수 호출시 결정된 객체타입에 맞게 실행됨.

def add(a, b):
    return a + b

c = add(1, 3.4)
d = add('dynamic', 'typing')
e = add(['list'], ['and', 'list'])
print(c)
print(d)
print(e)

#2-3 재귀적 함수 호출
#재귀 함수 : 함수 몸체에서 자기 자신을 호출하는 함수
 #수학에서 점화식과 유사한 코드
 #반드시 종결 조건 및 종결 조건이 만족할 때의 반환 값이 잇어야함.

#1부터 N까지 더하는 재귀 함수
def sum(N):
    if N == 1:
        return 1
    return N + sum(N-1)
print(sum(10))