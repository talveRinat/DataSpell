X = Ivanovallnew; % ������� ���������� ���������
t = IvanovresponseY'; % ������ ��������
X = normalize(X);
x = X';
MIN_N = 1; %����������� ����� �������� �� ������������ ���������
MAX_N =10; %������������ ����� �������� �� ������������ ���������

for i = MIN_N:MAX_N
 net = patternnet(i); %���������� ���� ������� ���������������
% net.layers{1}.transferFcn = 'tansig'; ����� �������� ��� �������� �
% ������ ����
% net.layers{2}.transferFcn = 'logsig';
[net,tr] = train(net,x,t); %�������� ���� � ��������

testX = x(:,tr.testInd);
testT = t(:,tr.testInd);

testY = net(testX);
[c,cm] = confusion(testT,testY);
Net(i) = 100*(1-c);
  
end
net = patternnet(3); %���������� ���� ������� ��������������� ��� ���������� ����� ��������
% net.layers{1}.transferFcn = 'tansig'; ����� �������� ��� �������� �
% ������ ����
% net.layers{2}.transferFcn = 'logsig';
[net,tr] = train(net,x,t); %�������� ���� � ��������
plotperform(tr) % ����������� �������� �������� ��������
nntraintool % ����� ���� �������� �������� ���� � ������� ������������ ���������� ��������
Net
testY = net(x);
[c1,cm1] = confusion(t,testY);
cm1
