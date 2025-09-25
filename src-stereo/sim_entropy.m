
a = [2,4,11,14]; %current context doors
b = [3,5,12,13]; %other context doors
c = [1,6,7,8,9,10,15,16]; %irrelevant doors

low = 2; 
mid = 50;
high = 100;
v = 3;

%case 1: person randomly picks doors
fprintf('equally chooses all doors\n');
x = zeros(16,16);
x = randi([-1,1],16,16)+mid;
get_entropy(x,a,v);

%case 2: picks correct and other-context doors
fprintf('equally chooses meta-task doors\n');
x = zeros(16,16);
x(a,a) = randi([-2 2],4,4)+mid;
x(b,b) = randi([-2 2],4,4)+mid;
x(a,b) = randi([-2 2],4,4)+mid;
x(b,a) = randi([-2 2],4,4)+mid;
get_entropy(x,a,v);

% case 3: picks correct doors
fprintf('equally chooses correct doors\n');
x = zeros(16,16);
x(a,a) = randi([-2 2],4,4)+mid;
get_entropy(x,a,v);

%case 4: person has consistent transitions
fprintf('prefers one transition for each correct door\n');
x = zeros(16,16);
x(a(1),a(2)) = randi([-2,2])+mid;
x(a(2),a(3)) = randi([-2,2])+mid;
x(a(3),a(4)) = randi([-2,2])+mid;
x(a(4),a(1)) = randi([-2,2])+mid;
get_entropy(x,a,v);

%case 5: person has extremely consistent transitions
fprintf('strongly prefers one transition for each correct door\n');
x = zeros(16,16);
x(a(1),a(2)) = randi([-2,2])+high;
x(a(2),a(3)) = randi([-2,2])+high;
x(a(3),a(4)) = randi([-2,2])+high;
x(a(4),a(1)) = randi([-2,2])+high;
get_entropy(x,a,v);

%case 6: person has consistent transitions for some doors
fprintf('prefers one transition for some correct doors\n');
x = zeros(16,16);
x(a(1),a(2)) = randi([-2 2])+mid;
x(a(2),a(3)) = randi([-2,2])+mid;
get_entropy(x,a,v);

%case 7: person has consistent transitions for correct doors, but also
%clicks others
fprintf('prefers one transition for correct doors, and clicks other context doors\n');
x = zeros(16,16);
x(b,b) = randi([-1,1])+low;
x(a,b) = randi([-1,1])+low;
x(b,a) = randi([-1,1])+low;
x(a(1),a(2)) = randi([-2 2])+mid;
x(a(2),a(3)) = randi([-2,2])+mid;
get_entropy(x,a,v);

function y = get_entropy(x,a,v)

if v==1
    x = x(a,a);
end

if v==2
    x = x(:,a);
end

y = x./(sum(x,1,'omitnan'));
y = y .* log2(y);
y = -sum(y,1,'omitnan');
disp(mean(y,'omitnan'))

end