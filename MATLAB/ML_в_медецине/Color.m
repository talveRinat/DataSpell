%�������������� ������������� ����������� � ������������� � ���������
%��������
% �������� ����������
myDir = uigetdir;
% ������ ����������� bmp
myFiles = dir(fullfile(myDir,'*.bmp'));
% ����������� �� ���� ������������
for k = 1:length(myFiles)
baseFileName = myFiles(k).name; % ����� ������ � �������� myDir
% ��������� ������ ���� � ����� �����
fullFileName = fullfile(myDir, baseFileName);
I = imread(fullFileName); % ��������� ���� ����������� 
[rows, columns, numberOfColorChannels] = size(I); % ���������� ������ ����������� ����������� 
% ��������� � ����������� ����� ������
if numberOfColorChannels == 1
       rgbimg=cat(3,I,I,I);% �������������� ����������� �� �������������� � �������������
            
 imwrite(rgbimg,fullFileName); % ���������� ����������� � ��� �� ������ � ��� �� ��������
end

end
