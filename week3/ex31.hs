
-- Excercise 3.1.1
xs0 = [1, 2, 3, 4, 5] type = [Integer]
xs1 [[1,2,3], [4,5]] type = [[Integer]]
xs2 = ['a','b','c'] type = [char]
xs3 = [] type = [a]
xs4 = [[],[]] type = [[a]]
xs5 = [[[]]] type = [[[a]]]
xs6 = [[]] type = [[a]]
xs7 = [[[]]] type = [[[a]]]

-- Excercise 3.1.2
xs0 = 1:2:3:4:5:[]
xs1 = (1:2:3:[]):[] ++ (4:5:[]):[]
xs2 = 'a':'b':'c':[]
xs3 = []
xs4 = []:[] ++ []:[]
xs5 = ([]:[]):[]
xs6 = []:[]
xs7 = ([]:[]):[]

-- Exercise 3.1.3
-- If we use the type command to get the type of [1,2,3] we get [num a] -> [a]
-- why is this different?
-- The type of [1,2,3] is [num a] -> [a] because the type of [1,2,3] is a list of numbers
-- and the type of [a] is a list of any type. We get a list of numbers but save it as a list of any type.

