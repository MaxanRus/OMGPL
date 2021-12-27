# Oh my god programming language

Питонисты плакали когда увидели это!

Реализован язык с типами int string. Я могу создавать переменные в любой степени вложенности, они корректно убираются со стека. Также корректно работает приоритет операций. Пример кода на моем языке:
```c++
string s = "abacaba ";
for (int i = 0; i < 5; i += 1) {
    string t = "";
    for (int j = 0; j < 3; j += 1) {
        t += "t";
    }
    s += t;
    print(s);
}

int x = (1 + 2) * 3;
print(x, x + 1, x + 2);
print(7777);

while (x != 0) {
    x -= 1;
}

print(x);
```

Результат работы:
```text
abacaba ttt 
abacaba tttttt 
abacaba ttttttttt 
abacaba tttttttttttt 
abacaba ttttttttttttttt 
9 10 11 
7777 
0 
END
```