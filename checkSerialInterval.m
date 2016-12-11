clc
clear all
close all

data = csvread('/Users/rnt22/Documents/HackoutCode/datasets/RotavirusEcuadorSIData.csv');

a = size(data);

lowerTimes = zeros(a(1), 1);
upperTimes = zeros(a(1), 1);

for i = 1:a(1)
    lowerTimes(i) = (data(i,1) + data(i,2))/2;
    upperTimes(i) = (data(i,3) + data(i,4))/2;
end

values = zeros(a(1),1);

for i = 1:a(1)
   values(i) = upperTimes(i) - lowerTimes(i);
end

hist(values,5)