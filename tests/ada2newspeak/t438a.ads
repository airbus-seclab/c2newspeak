with T438b;
package T438a is
 subtype TTT  is t438b.T;
 procedure MM ( F : out TTT ) renames T438b.S;
end T438a;
