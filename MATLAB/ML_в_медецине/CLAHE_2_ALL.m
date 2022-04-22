%ѕреобразование  изображений  в выбранном
%каталоге дл€ повышени€ контраста
% получаем директорию
myDir1 = uigetdir;
% только изображени€ .bmp
myFiles1 = dir(fullfile(myDir1,'*.bmp'));

% ѕробегаемс€ по всем изображени€м и повышаем контраст в соответствии с
% выбранным алгоритмом
for k = 1:length(myFiles1)
baseFileName1 = myFiles1(k).name; % имена файлов 
% добавл€ем полный путь к имени файла
fullFileName1 = fullfile(myDir1, baseFileName1);
L1=imread(fullFileName1);
L0 = L1 ( :, :, 1);

%ѕреобразование изображени€ в соответствии с алгоритмом CLAHE
L2 = adapthisteq(L0,'clipLimit',0.1,'Distribution', 'rayleigh','Alpha',0.2);
% «апись преобразованного изображени€ в каталоге программы
imwrite(L2,string(k)+'NEW.bmp');            
end


