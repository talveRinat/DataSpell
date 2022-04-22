clear all
load('Alexnet_1_2.mat'); %��������� ����
%��������� ������
imds = imageDatastore('DATA','IncludeSubfolders',true,'LabelSource','foldernames');
%����� ������ �� ��������� � ����������� �������
[imdsTrain,imdsValidation] = splitEachLabel(imds,0.7,'randomized');
N = 227; % ������ ����������� ��� ����
%������ �� ����� ������� �������� � ������ �������
augimdsTrain = augmentedImageDatastore([N N],imdsTrain);
augimdsValidation = augmentedImageDatastore([N N],imdsValidation);

%������ ����� �������� � ��������
options = trainingOptions('adam', ...     %�������������� ����������� ����� 
    'MiniBatchSize',20, ...     %���-�� ����������� � ������ ��������
    'MaxEpochs',10, ...         %���-�� ����
    'InitialLearnRate',3e-5, ...
    'Shuffle','once', ... %������������� ������
    'ValidationData',augimdsValidation, ... %������ ��� ��������
    'ValidationFrequency',5, ... %������� �������� ���� 
    'Verbose',false, ...    %����� ������ � ���. ������
    'ExecutionEnvironment', 'cpu', ... %���������� �������
    'Plots','training-progress');

%�������� ����
netTransfer = trainNetwork(augimdsTrain,layers_1,options);

%�������� ���� �� ��������
[YPred,probs] = classify(netTransfer,augimdsValidation);

A = [YPred { }];%����� �� ������ ����
B = imdsValidation.Labels;%�������� ����� 
N = size(A,1);
for k = 1:N
if string([A(k,1)]) == "NO"
  C(k)= 0  ;% ����� NO ����������� �������� 0
else
    C(k) = 1; % ����� YES ����������� �������� 0
  % ���������� ����������� � ��� �� ������ � ��� �� ��������
end
if string([B(k,1)]) == "NO" % ���������� ��� �������� �����
    D(k) = 0;
else 
    D(k) = 1;
end
end
%���������� TP,TN,FP,FN

TP=0; TN=0; FP=0; FN=0;
for k = 1:N
    TP = TP+C(k)*D(k);
    TN = TN+(1-C(k))*(1-D(k));
    FP = FP+C(k)*(1-D(k));
    FN = FN+(1-C(k))*D(k);
end
%���������� ����������������, �������������,��������
sensityve = TP/(TP+FN);
spesifice = TN/(TN+FP);
TP+FN
TN+FP
accuracy = mean(YPred == imdsValidation.Labels)
sensityve 
spesifice 

