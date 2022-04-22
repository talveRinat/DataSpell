A = csvread('Rinat_NEW_yes.csv');
B = csvread('Rinat_NEW_no.csv');
out3 = vertcat(A,B);
filename = 'Rinat_NEW_all.csv';
csvwrite(filename,out3)
