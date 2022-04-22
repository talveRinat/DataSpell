clear all
load('Alexnet_1_2.mat'); %загружаем сеть
%загружаем данные
imds = imageDatastore('DATA','IncludeSubfolders',true,'LabelSource','foldernames');
%делим данные на обучающую и проверочную выборки
[imdsTrain,imdsValidation] = splitEachLabel(imds,0.7,'randomized');
N = 227; % размер изображения для сети
%данные из обоих выборок приводим к одному формату
augimdsTrain = augmentedImageDatastore([N N],imdsTrain);
augimdsValidation = augmentedImageDatastore([N N],imdsValidation);

%задаем опции обучения и проверки
options = trainingOptions('adam', ...     %стохастический градиентный спуск 
    'MiniBatchSize',20, ...     %кол-во изображений в каждой итерации
    'MaxEpochs',10, ...         %кол-во эпох
    'InitialLearnRate',3e-5, ...
    'Shuffle','once', ... %перемешивание данных
    'ValidationData',augimdsValidation, ... %данные для проверки
    'ValidationFrequency',5, ... %частота проверки сети 
    'Verbose',false, ...    %вывод данных в ком. строку
    'ExecutionEnvironment', 'cpu', ... %аппаратные ресурсы
    'Plots','training-progress');

%обучение сети
netTransfer = trainNetwork(augimdsTrain,layers_1,options);

%проверка сети на точность
[YPred,probs] = classify(netTransfer,augimdsValidation);

A = [YPred { }];%метки на выходе сети
B = imdsValidation.Labels;%исходные метки 
N = size(A,1);
for k = 1:N
if string([A(k,1)]) == "NO"
  C(k)= 0  ;% метке NO присваиваем значение 0
else
    C(k) = 1; % метке YES присваиваем значение 0
  % сохранение изображения с тем же именем в том же каталоге
end
if string([B(k,1)]) == "NO" % аналогично для исходных меток
    D(k) = 0;
else 
    D(k) = 1;
end
end
%Определяем TP,TN,FP,FN

TP=0; TN=0; FP=0; FN=0;
for k = 1:N
    TP = TP+C(k)*D(k);
    TN = TN+(1-C(k))*(1-D(k));
    FP = FP+C(k)*(1-D(k));
    FN = FN+(1-C(k))*D(k);
end
%Определяем чувствительность, специфичность,точность
sensityve = TP/(TP+FN);
spesifice = TN/(TN+FP);
TP+FN
TN+FP
accuracy = mean(YPred == imdsValidation.Labels)
sensityve 
spesifice 

