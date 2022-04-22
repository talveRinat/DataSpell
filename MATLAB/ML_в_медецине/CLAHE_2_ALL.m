%��������������  �����������  � ���������
%�������� ��� ��������� ���������
% �������� ����������
myDir1 = uigetdir;
% ������ ����������� .bmp
myFiles1 = dir(fullfile(myDir1,'*.bmp'));

% ����������� �� ���� ������������ � �������� �������� � ������������ �
% ��������� ����������
for k = 1:length(myFiles1)
baseFileName1 = myFiles1(k).name; % ����� ������ 
% ��������� ������ ���� � ����� �����
fullFileName1 = fullfile(myDir1, baseFileName1);
L1=imread(fullFileName1);
L0 = L1 ( :, :, 1);

%�������������� ����������� � ������������ � ���������� CLAHE
L2 = adapthisteq(L0,'clipLimit',0.1,'Distribution', 'rayleigh','Alpha',0.2);
% ������ ���������������� ����������� � �������� ���������
imwrite(L2,string(k)+'NEW.bmp');            
end


