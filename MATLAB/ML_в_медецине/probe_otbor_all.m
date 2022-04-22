A = xlsread('Ivanov_all.xlsx');
[rows_A , cols_A] = size(A);
for i = 1:rows_A
   Y(i)= A (i,cols_A ) ;
    for j = 1:cols_A-1
    X(i,j)=A (i , j) ;
    end
end
Y = Y';
% X матрица признаков
% Y вектор откликов
X = normalize(X);

% отбор признаков алгоритмом relieff
[idxr,weights] = relieff(X,Y,5); 
c = cvpartition(Y,'k',4);
opts = statset('Display','iter');
fun = @(XT,YT,Xt,Yt)loss(fitcecoc(XT,YT),Xt,Yt);
[fs,history] = sequentialfs(fun,X,Y,'cv',c,'options',opts);
R_fs = size(fs, 2);
kr = 1;
for i = 1:R_fs
    if fs (1,i) == 1
    I_fs(kr) = i;
    kr = kr+1;
    end
end
%Вектор отобранных признаков
I_fs

% отбор признаков алгоритмом fscnca
mdl = fscnca(X,Y);
kf = 1;
I_md = mdl.FeatureWeights;
for i = 1:R_fs
    if I_md (i,1) > 0.5
    I_mdl(kf) = i;
    kf = kf+1;
    end
end
%Вектор отобранных признаков
I_mdl

% отбор признаков алгоритмом fscmrmr
[idx,scores] = fscmrmr(X,Y);
ks = 1;
for i = 1:R_fs
    if scores (1,i) > 0.25
    I_sc(ks) = i;
    ks = ks+1;
    end
end
%Вектор отобранных признаков
I_sc

% отбор признаков алгоритмом Lasso
[B Stats] = lasso(X,Y,'CV',5);
NS = Stats.Index1SE;
kL = 1;
for i = 1:R_fs
    if abs(B (i,NS)) > 0
    I_L(kL) = i;
    kL = kL+1;
    end
end
%Вектор отобранных признаков
I_L
I_all = horzcat(I_fs,I_mdl,I_sc,I_L);
B = I_all;
C =  unique(B); % записываем отличающиеся числа из B
X = normalize(X);
[rows_C , cols_C] = size(C);
[rows_X , cols_X] = size(X);
for i = 1:rows_X
    for j = 1:cols_C
    Xnew(i,j)=X (i , C( j)) ;
    Xnew(i,cols_C+1)= Y (i ) ;
    end
end
filename = 'Ivanov_all_NEW.xlsx';
xlswrite(filename,Xnew);
