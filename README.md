# Методы принятия решений

# Постановка задачи
 В пространстве объектов заданна метрика растояния, характеризующая степень близости объектов. Нам дана обучающая выборка и объект, который нужно отнести к одному из существующих классов. Решать задачу мы будем при помощи обучающей выборки "Ирисы Фишера".
Данная выборка содержит 150 объектов-ирисов: по 50 объектов каждого из трех классов. Ирыс представлен четыремя признаками: длинной и шириной чашелистника и лепестка. 
  
## 1. Метрические алгоритмы классификации
**Метрические методы обучения** - методы, основанные на анализе сходства объектов, а метрические алгоритмы классификации опираются на гипотезу компактности: схожим объектам соответствуют схожие ответы. 
Для поиска оптимальных параметров для каждого из рассматриваемых ниже метрических алгоритмов используется **LOO -- leave-one-out** (критерий скользящего контроля), который состоит в следующем:

1. Исключать по одному объекту  из выборки, получится новая выборка без исключенных объектов (назовём её Xl_1).
2. Запускать алгоритм от объекта, который нужно классифицировать, на новой выборке Xl_1.
3. Завести переменную ошибки, и, когда алгоритм ошибается, Q = Q + 1 (изначально Q = 0).
4. Когда все объекты будут перебраны, вычислить LOO, частное от ошибки и количества объектов выборки.
Оптимальный алгоритм получим при минимальном скользящем контроле (LOO).
Преимущества LOO в том, что каждый объект ровно один раз участвует в контроле, а длина обучающих подвыборок лишь на единицу меньше длины полной выборки.
Недостатком LOO является большая ресурсоёмкость, так как обучаться приходится L раз.

## 1.1. Алгоритм ближайших соседей
### 1NN
1. На первом шаге подбираем метрику (в данном случае это евклидово пространство). 
2. Считаем расстояние от классифицируемого объекта, до объектов выборки, заносим значения в массив расстояний. 
3. Сортируем масив по возрастанию (от бижнего элемента к дальнему). 
4. Находим класс первого элемента массива, и относим классифицируеммый объект к этому классу.

Результат работы алгоритма:
  
 ![](https://github.com/Abkelyamova/-/blob/master/1nnRplot.png)
 
 ### KNN
 
Алгоритм выбирает _k_ ближайших соседей, возвращает тот класс, который среди выбранных встречается большее количество раз и относит классифицируемый объект *u* этому классу.
Для оценки близости объекта *u* к классу *y* алгоритм **kNN** использует следующую функцию: ![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D) , где *i* -- порядок соседа по расстоянию к классифицируемому объекту u.
LOO для KNN показал что оптимальное k = 6.

#### Преимущества:

- При *k*, подобранном около оптимального, алгоритм "неплохо" классифицирует.

#### Недостатки:
- Нужно хранить всю выборку.
- При *k = 1* неустойчивость к погрешностям (*выбросам* -- объектам, которые окружены объектами чужого класса), вследствие чего этот выброс классифицировался неверно и окружающие его объекты, для которого он окажется ближайшим, тоже.
- При *k = l* алгоритм наоборот чрезмерно устойчив и вырождается в константу.
- Максимальная сумма объектов в *counts* может достигаться в нескольких классах одновременно.
- Точки, расстояние между которыми одинаково, не все будут учитываться.
#### Результат работы алгоритма:
![](https://github.com/Abkelyamova/-/blob/master/KNN%26LOO.png)
### Алгоритм k взвешеных ближайших соседей (kwnn)
Данный алгоритм классификации относит объект *u* к тому классу *y*, у которого максимальна сумма весов из его *k* соседей. То есть помимо расстояния в этом алгаритме используется еще функция веса для ближайших элементов выборки.



