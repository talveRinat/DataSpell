%ѕреобразование одноканальных изображений в трехканальные в выбранном
%каталоге
% получаем директорию
myDir = uigetdir;
% только изображени€ bmp
myFiles = dir(fullfile(myDir,'*.bmp'));
% ѕробегаемс€ по всем изображени€м
for k = 1:length(myFiles)
baseFileName = myFiles(k).name; % имена файлов в каталоге myDir
% добавл€ем полный путь к имени файла
fullFileName = fullfile(myDir, baseFileName);
I = imread(fullFileName); % считываем файл изображени€ 
[rows, columns, numberOfColorChannels] = size(I); % определ€ем размер изображени€ изображени€ 
% ѕримен€ем к изображению серый фильтр
if numberOfColorChannels == 1
       rgbimg=cat(3,I,I,I);% преобразование изображени€ из одноканального в трехканальное
            
 imwrite(rgbimg,fullFileName); % сохранение изображени€ с тем же именем в том же каталоге
end

end
