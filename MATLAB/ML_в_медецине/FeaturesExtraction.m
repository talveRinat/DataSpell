
myDir = uigetdir('/Users/rinatmahmutov/Documents/MATLAB/');

myFiles = dir(fullfile(myDir,'*.bmp'));
out = []
out1 = []
out11 = []

for k = 1:length(myFiles)
baseFileName = myFiles(k).name;

fullFileName = fullfile(myDir, baseFileName);
I = imread(fullFileName);
[rows, columns, numberOfColorChannels] = size(I);

if numberOfColorChannels > 1
I = rgb2gray(I);
end
% a) G=128, d=1 
glcm0 = graycomatrix(I, 'NumLevels', 128, 'Offset', [0 1], 'Symmetric', true);
glcm1 = graycomatrix(I, 'NumLevels', 128, 'Offset', [-1 1], 'Symmetric', true);
glcm2 = graycomatrix(I,'NumLevels', 128, 'Offset', [-1 0], 'Symmetric', true);
glcm3 = graycomatrix(I,'NumLevels', 128, 'Offset', [-1 -1], 'Symmetric', true);

xFeatures = 1:14;
x = haralickTextureFeatures(glcm0, 1:14);
y = haralickTextureFeatures(glcm1, 1:14);
z = haralickTextureFeatures(glcm2, 1:14);
t = haralickTextureFeatures(glcm3, 1:14);
a = transpose(cat(1,x,y,z,t));

out = [out;a];
end
for k = 1:length(myFiles)
    baseFileName = myFiles(k).name;

fullFileName = fullfile(myDir, baseFileName);
I = imread(fullFileName);
[rows, columns, numberOfColorChannels] = size(I);

if numberOfColorChannels > 1
I = rgb2gray(I);
end
% b)G=128, d=7
glcm00 = graycomatrix(I, 'NumLevels', 128, 'Offset', [0 7], 'Symmetric', true);
glcm11 = graycomatrix(I, 'NumLevels', 128, 'Offset', [-7 7], 'Symmetric', true);
glcm22 = graycomatrix(I,'NumLevels', 128, 'Offset', [-7 0], 'Symmetric', true);
glcm33 = graycomatrix(I,'NumLevels', 128, 'Offset', [-7 -7], 'Symmetric', true);



x1 = haralickTextureFeatures(glcm00, 1:14);
y1 = haralickTextureFeatures(glcm11, 1:14);
z1 = haralickTextureFeatures(glcm22, 1:14);
t1 = haralickTextureFeatures(glcm33, 1:14);
a1 = transpose(cat(1,x1,y1,z1,t1));

out1 = [out1;a1];
end
size(out)
size(out1)
out2 = horzcat(out,out1);
size(out2)

[N, M]=size(out2);
for i = 1:N
    Y11(i)=-1; % no -1, yes 1
end
out3 = horzcat(out2,transpose(Y11));

filename = 'Rinat_NEW_no.xlsx';
xlswrite(filename,out3)
