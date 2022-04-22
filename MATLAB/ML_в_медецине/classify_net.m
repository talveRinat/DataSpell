X = Ivanovallnew; % матрица отобранных признаков
t = IvanovresponseY'; % вектор откликов
X = normalize(X);
x = X';
MIN_N = 1; %минимальное число нейронов из исследуемого диапазона
MAX_N =10; %максимальное число нейронов из исследуемого диапазона

for i = MIN_N:MAX_N
 net = patternnet(i); %определяем сеть прямого распространения
% net.layers{1}.transferFcn = 'tansig'; можно задавать вид сигмоиды в
% каждом слое
% net.layers{2}.transferFcn = 'logsig';
[net,tr] = train(net,x,t); %передача сети в обучение

testX = x(:,tr.testInd);
testT = t(:,tr.testInd);

testY = net(testX);
[c,cm] = confusion(testT,testY);
Net(i) = 100*(1-c);
  
end
net = patternnet(3); %определяем сеть прямого распространения для выбранного числа нейронов
% net.layers{1}.transferFcn = 'tansig'; можно задавать вид сигмоиды в
% каждом слое
% net.layers{2}.transferFcn = 'logsig';
[net,tr] = train(net,x,t); %передача сети в обучение
plotperform(tr) % отображение качества процесса обучения
nntraintool % вызов окна процесса обучения сети в котором отображаются результаты обучения
Net
testY = net(x);
[c1,cm1] = confusion(t,testY);
cm1
