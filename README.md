# Методы принятия решений

## Навигация

- [Метрические алгоритмы классификации](#Метрические-алгоритмы-классификации)
  - [Алгоритм ближайших соседей](#Алгоритм-ближайших-соседей)
  - [Алгоритм k взвешенных ближайших соседей](#Алгоритм-k-взвешенных-ближайших-соседей)
  - [Метод парзеновского окна](#Метод-парзеновского-окна)
  - [Сравнение алгоритмов классификации](#Сравнение-алгоритмов-классификации)
- [Байесовские алгоритмы классификации](#Байесовские-алгоритмы-классификации)
  - [Подстановочный алгоритм Plug-in](#Подстановочный-алгоритм-Plug-in)
  - [Линейный дискриминант Фишера](#Линейный-дискриминант-Фишера)
-[Линейные алгоритмы классификации](#Линейные-алгоритмы-классификации)
  - [Метод-стохастического-градиента](#Метод-стохастического-градиента)
    - [ADALINE](#ADALINE)
    - [Персептрон Розенблатта](#Персептрон-Розенблатта)
    - [Логистическая регрессия](#Логистическая-регрессия)
# Постановка задачи
 В пространстве объектов задаем функцию расстояния, характеризующая степень близости объектов. Нам дана обучающая выборка и объект, который нужно отнести к одному из существующих классов (классифицировать). Решать задачу мы будем при помощи обучающей выборки "Ирисы Фишера".
Данная выборка содержит 150 объектов-ирисов: по 50 объектов каждого из трех классов. Ирис представлен четырьмя признаками: длинной и шириной чашелистика и лепестка. 
  
# Метрические алгоритмы классификации
**Метрические методы обучения** - методы, основанные на анализе сходства объектов. Метрические алгоритмы классификации опираются на гипотезу компактности: схожим объектам соответствуют схожие ответы. 
Для поиска оптимальных параметров для каждого из рассматриваемых ниже метрических алгоритмов используется **LOO -- leave-one-out** (критерий скользящего контроля), который состоит в следующем:

1. Исключать по одному объекту  из выборки, получаем новую выборку без исключенных объектов (назовём её Xl_1).
2. Запускать алгоритм от объекта, который нужно классифицировать, на новой выборке Xl_1.
3. Завести переменную ошибки, и, когда алгоритм ошибается, Q = Q + 1 (изначально Q = 0).
4. Когда все объекты будут перебраны, вычислить LOO, частное от ошибки и количества объектов выборки.
Оптимальный алгоритм получим при минимальном скользящем контроле (LOO).
Преимущества LOO в том, что каждый объект ровно один раз участвует в контроле, а длина обучающих подвыборок лишь на единицу меньше длины полной выборки.
Недостатком LOO является большая ресурсоёмкость, так как обучаться приходится L раз.

## Алгоритм ближайших соседей
### 1NN
1. На первом шаге подбираем метрику (в данном случае это евклидово пространство). 
2. Считаем расстояние от классифицируемого объекта, до объектов выборки, заносим значения в массив расстояний. 
3. Сортируем масив по возрастанию (от ближнего элемента к дальнему). 
4. Находим класс первого элемента массива, и относим классифицируемый объект к этому классу.

Результат работы алгоритма:
  
 ![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/NN.png)
 
 ### KNN
 
Алгоритм выбирает _k_ ближайших соседей, возвращает тот класс, который среди выбранных встречается большее количество раз и относит классифицируемый объект *u* этому классу.
Для оценки близости объекта *u* к классу *y* алгоритм **kNN** использует следующую функцию: ![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D) , где *i* -- порядок соседа по расстоянию к классифицируемому объекту u.
LOO для KNN показал что оптимальное k = 6.
Результат работы алгоритма: 
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/6NN.png)

Скользящий контроль для алгоритма KNN:
```diff
LOO <- function(xl,class) 
{
  n <- dim(xl)[1];
  loo <- rep(0, n-1) 
  for(i in 1:n)
  {
    X <- xl[-i, 1:3]
    u <- xl[i, 1:2]
    orderedXl <- sortObjectByDist(X, u)
    for(k in 1:(n-1))
    {
      test <- knn(X,u,k,orderedXl)
      if(colors[test] != colors[class[i]])
      {
        loo[k] <- loo[k]+1;
      }
    }
  }
```

![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/loo_knn.png)
#### Преимущества:

- При *k*, подобранном около оптимального, алгоритм "неплохо" классифицирует.

#### Недостатки:
- Нужно хранить всю выборку.
- При *k = 1* неустойчивость к погрешностям (*выбросам* -- объектам, которые окружены объектами чужого класса), вследствие чего этот выброс классифицировался неверно и окружающие его объекты, для которого он окажется ближайшим, тоже.
- При *k = l* алгоритм наоборот чрезмерно устойчив и вырождается в константу.
- Максимальная сумма объектов в *counts* может достигаться в нескольких классах одновременно.
- Точки, расстояние между которыми одинаково, не все будут учитываться.

## Алгоритм k взвешенных ближайших соседей
### KwNN
Данный алгоритм классификации относит объект *u* к тому классу *y*, у которого максимальна сумма весов из его *k* соседей, то есть объект относится к тому классу, который набирает больший суммарный вес среди k ближайших соседей.
В данном алгоритме, помимо функции расстояния, используется весовая функция, которая оценивает степень важности при классификации заданного объекта к какому-либо классу, что и отличает его от алгоритма kNN.

Результат роботы алгоритма:
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/KwNN.png)
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/LOO%26Kwnn.png)
#### Недостатки:
1. Приходится хранить обучающую выборку Xl целиком, что приводит к неэффективному расходу памяти. При наличии погрешностей это может привести к понижению точности классификации вблизи границ классов.
2. Исключается настройка алгоритмов по данным (крайне "бедный" набор параметров).
3. Если суммарные веса классов оказываются одинаковыми, то алгоритм относит классифицируемый объект u к любому из классов.
#### Преимущества:
При любом k алгоритм неплохо классифицирует. 
#### Чем kwnn лучше/хуже knn?
- Шире диапазон оптимальных k.
- Лучше точность на границах.

Пример показывающий преимущество метода kwNN над kNN(k=7): 
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/knn&kwnn.png)
## Метод парзеновского окна

Вокруг классифицируемого объекта строится окно радиуса h. Считаем расстояния от него до тех элементов, которые попали в окно. Придаем им веса (чем меньше расстояние, тем больше вес), суммируем веса по классам и записываем в массив. Определяем класс с максимальной суммой весов. И классифицируемый объект определяем к этому классу.
В отличие от алгоритма kwNN, в методе парзеновского окна в качестве функции веса используется различные ядра.  Также с помощью _скользящего контроля(LOO)_, необходимо подбирать параметр «ширина окна». Надо выбрать такое _h_, которое даст меньше всего ошибок. 

Рассмотрим результаты работы алгоритма. Чаще всего применяются 5 типов ядер:

Случай прямоугольного ядра:

![](https://latex.codecogs.com/gif.latex?R(r)=\frac{1}{2}[|r|\leq&space;1])

![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/LOO_PW(R).png)
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/pw(pg).png)

Случай треугольного ядра:
![](https://latex.codecogs.com/gif.latex?T(r)=(1-|r|)\cdot&space;[|r|\leq&space;1])
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/LOO_PW(t).png)
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/PW(T).png)

Случай квартического ядра:
![](https://latex.codecogs.com/gif.latex?Q(r)=\frac{15}{16}(1-r^{2})^{2}\cdot&space;[|r|\leq&space;1])
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/LOO_PW(Q).png)
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/PW(Q1).png)

Случай ядра Епанечникова:
![](https://latex.codecogs.com/gif.latex?E(r)=\frac{3}{4}(1-r^{2})\cdot&space;[|r|\leq&space;1])
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/LOO_PW(EP).png)
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/PW(EPACH).png)

Случай гауссовского ядра, (нормальное распределение):
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/LOO_PW(G).png)
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/PW(G).png)

Вывод: Больше всего подходит гауссовское ядро. Оно однозначно разделило классы на всей плоскости.
В остальных случаях все точки, не попавшие в окно, отсеились (в ядрах кроме гауссовского). 

## Сравнение алгоритмов классификации:
<table>
<tr><td>Метод</td><td>параметры</td><td>величина ошибок</td><tr>
<tr><td> KNN</td><td>k=6</td><td>0.033</td><tr>
<tr><td> KWNN</td><td>k=9</td><td>0.033</td<tr>
<tr><td> Парзеновское окно
(Прямоугольное ядро)</td><td>h=0.4</td><td>0.04</td<tr>
  <tr><td> Парзеновское окно
(Треугольное ядро)</td><td>h=0.4</td><td>0.04</td<tr>
    <tr><td> Парзеновское окно
(Квартическое ядро)</td><td>h=0.4</td><td>0.04</td<tr>
<tr><td> Парзеновское окно
(Ядро Епанечникова)</td><td>h=0.4</td><td>0.04</td<tr>
  <tr><td> Парзеновское окно
(Гауссовское ядро)</td><td>h=0.1</td><td>0.04</td<tr>
 </table>


# Байесовские алгоритмы классификации
   Байесовский подход основан на следующей теореме: *если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде*.  
   Чтобы классифицировать объект __a(x)__, по известному вектору признаков __x__, для начала, нужно вычислить функции правдоподобия каждого из классов, затем вычислить апостериорные вероятности классов. Классифицируемый объект относится к тому классу, у которого апостериорная вероятность максимальна.  
   
   **Байесовское решающее правило**:  
![](https://camo.githubusercontent.com/cfb0e23e2816ceaed4e85bec306e0b71d1f16ceb/68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f61253238782532392532302533442532306172672532302535436d61785f25374279253230253543657073696c6f6e253230592537442535436c616d6264612532305f25374279253744502532387925323925354372686f2532387825374379253239)
   
Если __P(y)__ одинаковы для всех классов - мы просто выбираем класс, плотность которого больше в точке __x__.

## Линии уровня многомерного нормального распределения 
Это специальный случай баесовской классификации, когда предполагается, что плотности всех классов
![](http://latex.codecogs.com/svg.latex?p_y%28x%29%2C%20y%20%5Cin%20Y)
являются многомерными нормальными. В этом случае задача решается аналитически.
Сами плотности вычисляются по формуле:

![](http://latex.codecogs.com/svg.latex?N%28x%3B%5Cmu%2C%5CSigma%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%282%5Cpi%29%5En%20%7C%5CSigma%7C%7D%7D%20%5Ccdot%20exp%5Cleft%28-%5Cfrac%7B1%7D%7B2%7D%28x%20-%20%5Cmu%29%5ET%20%5CSigma%5E%7B-1%7D%20%28x%20-%20%5Cmu%29%5Cright%29),
в которой

![](http://latex.codecogs.com/svg.latex?x%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– объект, состоящий из *n* признаков,

![](http://latex.codecogs.com/svg.latex?%5Cmu%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– математическое ожидание,

![](http://latex.codecogs.com/svg.latex?%5CSigma%20%5Cin%20%5Cmathbb%7BR%7D%5E%7Bn%20%5Ctimes%20n%7D)
– ковариационная матрица (положительно определенная, симметричная, невырожденная).


## Подстановочный алгоритм Plug-in

Алгоритм __Plug-in__ еще один случай байесовского классификатора который обладает многомерным нормальным распределением.

Чтобы узнать плотности распределения классов, алогритм оценивает неизвестные параметры по частям для каждого класса :

![](https://latex.codecogs.com/gif.latex?\hat{\mu&space;}=\tfrac{1}{m}&space;\sum_{i=1}^{m}x_{i})  

![](https://latex.codecogs.com/gif.latex?\hat{\Sigma&space;}=\frac{1}{m-1}\sum_{i=1}^{m}(x_{i}-\hat{\mu&space;})(x_{i}-\hat{\mu&space;})^{T})  

где  𝜇 ∈ ℝ - математическое ожидание (центр), а  𝛴 ∈ ℝ - ковариационная матрица (симметричная, невырожденная, положительно определённая).  
Алгоритм заключается в том, чтобы найти неизвестные параметры 𝜇 и  𝛴  для каждого класса y и подставить их в формулу оптимального байесовского классификатора. В данном алгоритме мы предполагаем, что ковариационные матрицы не равны. 

Выбирая различные матрицы ковариации и центры для генерации тестовых данных, будем получать различные виды
дискриминантной функции.

Пример 1. Дискриминантная функфия - эллипс. 

Центр первого класса (2,2) Ковариационная матрица первого класса: ![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/CodeCogsEqn(2).gif)

Центр второго класса (15,2) Ковариационная матрица второго класса ![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/CodeCogsEqn(3).gif)

Результат: 

![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/окружность.png)

Пример 2. Дискриминантная функфия - гипербола.

Центр первого класса (1,2) Ковариационная матрица первого класса: ![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/CodeCogsEqn(4).gif)

Центр второго класса (5,2) Ковариационная матрица второго класса: ![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/CodeCogsEqn(5).gif)

Результат: 

![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/гипербола.png)

Пример 3. Дискриминантная функфия - парабола.

Центр первого класса (-5,0) Ковариационная матрица первого класса:![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/CodeCogsEqn.gif)

Центр второго класса (10,0) Ковариационная матрица второго класса:![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/CodeCogsEqn(1).gif)

Результат:

![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/парабола.png)

### Линейный дискриминант Фишера

__ЛДФ__ основан на __подстановочном алгоритме__ с предположением,
что ковариационные матрицы классов равны. Отсюда следует, что
разделяющая поверхность вырождается в прямую. Это условие в
__plug-in__ не выполнялось, так как разделяющая поверхность все равно
была квадратичной (хоть и приближенной к прямой). Отсюда следует,
что __ЛДФ__ должен иметь более высокое качество классификации при
одинаковых ковариационных матрицах.

Программно алгоритм отличается от __подстановочного__ пересчетом
_ковариационной матрицы_ и поиском _разделяющей поверхности_.

Так как матрицы равны, можем оценить их все вместе:

![](http://latex.codecogs.com/svg.latex?%5Chat%7B%5CSigma%7D%20%3D%20%5Cfrac%7B1%7D%7Bl%20-%20%7CY%7C%7D%20%5Ccdot%20%5Csum_%7Bi%3D1%7D%5E%7Bl%7D%28x_i%20-%20%5Chat%7B%5Cmu%7D_y_i%29%28x_i%20-%20%5Chat%7B%5Cmu%7D_y_i%29%5ET)

Результат работы алгоритма:
![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Bayes_algoritms/Rplot08.png)


ЛДФ отличный алгоритм, с линейной разделительной поверхностью принятия решений и хорошей обобщающей способностью. 

# Линейные алгоритмы классификации

Линейный классификатор основан на принципе разделения классов. Будем рассматривать задачу классификации с двумя видами классов ![lin](https://user-images.githubusercontent.com/44859059/50448534-ed8d8380-0932-11e9-94c8-bd751fadd03c.png). 

Алгоритм ![lin2](https://user-images.githubusercontent.com/44859059/50448620-4e1cc080-0933-11e9-861f-fad7685c32ee.png) называется линейным алгоритмом классификации, в котором _w_ - вектор параметров который, а _f(x, w)_ - дискриминантная функция. Если f(x, w) > 0, то алгоритм a относит объект x к классу +1, иначе к классу −1. Уравнение _f(x, w)=0_ задает разделяющую прямую. 
Величина *M(w)=yf(x, w)* называется отступом(Margine). Если отступ объекта меньше нуля на объекте *x*, то алгоритм на этом объекте ошибается. Чем больше величина отступа тем лучше работает алгоритм.

Эмпирический риск определяется следующей формулой: 

![default](https://user-images.githubusercontent.com/44859059/50449956-b40d4600-093b-11e9-9a60-1ad3b88cf14f.png)

Функция *L(M)* называется фукцией потерь, она непрерывна и неотрицательна. Наша цель -  минимизация функции потерь.

## Метод стохастического градиента  

Пусть дана обучающая выборка. 
Нужно найти вектор параметров *w*, при котором достигается минимум эмпирического риска:
 
Для этого применим метод градиентного спуска:  

1.выберем приближенное значение для вектора параметров *w*;  

2.запускаем итерационный процесс, на каждом шаге которого сдвигаемся в сторону, противоположную вектору
градиента 𝑄′(𝑤, 𝑋ℓ)) до тех пор, пока вектор весов 𝑤 не перестанет изменяться. Вычисления градиента производится не на всех
объектах обучения, а выбирается случайный объект, на основе которого и происходят вычисления.  
При использовании метода стохастического градиента необходимо нормализовать исходные данные.
**В зависимости от функции потерь, которая используется в функционале эмпирического риска, будем получать различные
линейные алгоритмы классификации.**  

## ADALINE

Данный алгоритм использует квадратичную функцию потерь:

![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29%3D%28M-1%29%5E2%3D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i-1%29%5E2)
и _дельта-правило_ правило обновления весов
![](http://latex.codecogs.com/svg.latex?w%3Dw-%5Ceta%28%5Clangle%20w%2Cx_i%20%5Crangle-y_i%29x_i).
Реализация алгоритма <a href="https://abkelyamova.shinyapps.io/adaline/">ADALINE</a>
## Персептрон Розенблатта

Имеет _кусочно-линейную функцию потерь_
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%3D%28-M%29_&plus;%3D%5Cmax%28-M%2C0%29)
и _правило Хебба_ для обновления весов
![](http://latex.codecogs.com/svg.latex?%5Ctext%7Bif%20%7D%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%3C0%20%5Ctext%7B%20then%20%7D%20w%3A%3Dw&plus;%5Ceta%20x_iy_i).
Реализация линейного классификатора <a href="https://abkelyamova.shinyapps.io/lines/">Персептрон Розенблатта</a>
## Логистическая регрессия

Имеет _логистическую функцию потерь_
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29%20%3D%20%5Clog_2%281%20&plus;%20e%5E%7B-M%7D%29)
и _логистическое_ правило обновления весов
![](http://latex.codecogs.com/svg.latex?w%20%3A%3D%20w&plus;%5Ceta%20y_ix_i%5Csigma%28-%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)
, где
![](http://latex.codecogs.com/svg.latex?%5Csigma%28z%29%3D%5Cfrac%7B1%7D%7B1&plus;e%5E%7B-z%7D%7D)
– _сигмоидная функция_.
Реализация линейного классификатора <a href="https://abkelyamova.shinyapps.io/linii/">Логистическая регрессия</a>
### Вывод
Для сравнения линейных классификаторов, использующие метод стохастического градиента, была реализована программа отображающая сразу три разделяющие прямые:  

![](https://github.com/Abkelyamova/SMPR_AbkelyamovaGulzara/blob/master/Lines_algoritms/all_in.png)

На изображении видно что все алгоритмы ведут себя практически одинаково. Однако логистическа ярегрессия дает лучшие результаты  по сравнению с дельта-правилом(ADALINE) и правилом Хэбба(поскольку она использует „более правильную” функцию потерь). 
