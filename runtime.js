"use strict";
function array_p(v){return (Array.isArray(v));};
function tag(a){return a[0];};
var PAIR=["pair"];
var VECTOR=["vector"];
var IMMUTABLE_VECTOR=["immutable-vector"];
var CHAR=["char"];
var MUTABLE_STRING=["mutable-string"];
var BYTES=["bytes"];
var MUTABLE_BYTES=["mutable-bytes"];
var SYMBOL=["symbol"];
var KEYWORD=["keyword"];
var VOID=[];
var NULL=[];
function boolean_p(v){return ((typeof(v))==="boolean");};
function equal_p(v1,v2){return (v1===v2);};
function eqv_p(v1,v2){return (v1===v2);};
function eq_p(v1,v2){return (v1===v2);};
function number_p(v){return ((typeof(v))==="number");};
function exact_integer_p(v){return (((typeof(v))==="number")&&(v===(+v))&&(v===(v|0)));};
function string_p(v){return (((typeof(v))==="string")||((array_p(v))&&((tag(v))===MUTABLE_STRING)));};
function immutable_string_p(v){return ((typeof(v))==="string");};
function mutable_string_p(v){return ((array_p(v))&&((tag(v))===MUTABLE_STRING));};
function make_string(k,ch){return ((function(t){return (((false||(t===1))===false)?(((false||(t===2))===false)?((function(){return (error("make-string","expected at one or two argument"));})()):((function(){return (make_string2(k,ch));})())):((function(){return (make_string1(k));})()));})(arguments.length));};
function make_string1(k){return (make_string2(k," "));};
function make_string2(k,ch){var a=(Array((k+1)));(a[0]=MUTABLE_STRING);{var i=1,t=(k+1);while((true&&true&&(i<t))){((function(i_1){(a[i_1]=ch[1]);return (i+=1);})(i));};undefined;}return a;};
function make_primitive_string(n,c){var s="";while(true){if(!((!((n&1)===0))===false)){(s+=c);}else ;;(n>>=1);if(!((n===0)===false))break;else (c+=c);;};return s;};
function string__gimmutable_string(s){return ((((typeof(s))==="string")===false)?((true===false)?undefined:((function(){var n_a1=s.length,n=(n_a1-1),a=(Array(n));{var i=0,t=n;while((true&&true&&(i<t))){((function(i_2){(a[i_2]=s[(i_2+1)]);return (i+=1);})(i));};undefined;}return (String((a.join(""))));})())):((function(){return s;})()));};
function immutable_string__gstring(str_3){var n=str_3.length,a=(Array((n+1)));(a[0]=MUTABLE_STRING);{var i=0,t=n;while((true&&true&&(i<t))){((function(i_4){(a[(i_4+1)]=str_3[i_4]);return (i+=1);})(i));};undefined;}return a;};
function string(){var args=arguments,n=args.length;var a=(Array((n+1)));(a[0]=MUTABLE_STRING);{var i=0,t=n;while((true&&true&&(i<t))){((function(i_5){(a[(i_5+1)]=args[i_5][1]);return (i+=1);})(i));};undefined;}return a;};
function string_length(s){return ((((typeof(s))==="string")===false)?(s.length-1):s.length);};
function string_ref(s,i){return ((((typeof(s))==="string")===false)?[CHAR,s[(i+1)]]:[CHAR,s[i]]);};
function string_set_e(s,i,c){return (s[(i+1)]=c[1]);};
function substring3(str_6,start,end){return (str_6.substring(start,end));};
function substring2(str_7,start){return (str_7.substring(start,(string_length(str_7))));};
function substring(){return ((function(t){return (((false||(t===2))===false)?(((false||(t===3))===false)?((function(){return (error("substring","expected two to three arguments"));})()):((function(){return (substring3(arguments[0],arguments[1],arguments[2]));})())):((function(){return (substring2(arguments[0],arguments[1]));})()));})(arguments.length));};
function string_eq_p(str1,str2){return (((immutable_string_p(str1))===false)?(((immutable_string_p(str2))===false)?((string__gimmutable_string(str1))===(string__gimmutable_string(str2))):((string__gimmutable_string(str1))===str2)):(((immutable_string_p(str2))===false)?(str1===(string__gimmutable_string(str2))):(str1===str2)));};
function string_l_p(str1,str2){return (((immutable_string_p(str1))===false)?(((immutable_string_p(str2))===false)?((string__gimmutable_string(str1))<(string__gimmutable_string(str2))):((string__gimmutable_string(str1))<str2)):(((immutable_string_p(str2))===false)?(str1<(string__gimmutable_string(str2))):(str1<str2)));};
function string_g_p(str1,str2){return (((immutable_string_p(str1))===false)?(((immutable_string_p(str2))===false)?((string__gimmutable_string(str1))>(string__gimmutable_string(str2))):((string__gimmutable_string(str1))>str2)):(((immutable_string_p(str2))===false)?(str1>(string__gimmutable_string(str2))):(str1>str2)));};
function string_l_eq_p(str1,str2){return (((immutable_string_p(str1))===false)?(((immutable_string_p(str2))===false)?((string__gimmutable_string(str1))<=(string__gimmutable_string(str2))):((string__gimmutable_string(str1))<=str2)):(((immutable_string_p(str2))===false)?(str1<=(string__gimmutable_string(str2))):(str1<=str2)));};
function string_g_eq_p(str1,str2){return (((immutable_string_p(str1))===false)?(((immutable_string_p(str2))===false)?((string__gimmutable_string(str1))>=(string__gimmutable_string(str2))):((string__gimmutable_string(str1))>=str2)):(((immutable_string_p(str2))===false)?(str1>=(string__gimmutable_string(str2))):(str1>=str2)));};
function string_upcase(str_8){return (((immutable_string_p(str_8))===false)?(string_upcase((string__gimmutable_string(str_8)))):(immutable_string__gstring((String((str_8.toUpperCase()))))));};
function string_downcase(str_9){return (((immutable_string_p(str_9))===false)?(string_downcase((string__gimmutable_string(str_9)))):(immutable_string__gstring((String((str_9.toLowerCase()))))));};
function string__glist(s){return ((function(xs){{var s_10=s,n=s_10.length,i=0;while((true&&true&&(i<n))){((function(c){(xs=(cons(c,xs)));return (i+=1);})(s_10[i]));};undefined;}return (reverse(xs));})(NULL));};
function string_append2(str1,str2){return (str1.concat(str2));};
function string_append(){var args=arguments;var as_11=((function(){var n=args.length,a=new Array(n),i=0;{var a_12=args,n_13=a_12.length,i_14=0;while((true&&true&&(i_14<n_13))){((function(a_16){(a[i]=a_16);(i+=1);return (i_14+=1);})(a_12[i_14]));};undefined;}if(!((i<n)===false)){var i_15=i,t=n;while((true&&true&&(i_15<t))){((function(j){a[j]=0;return (i_15+=1);})(i_15));};undefined;}else ;;return a;})());return (as_11.join(""));};
function bytes_p(v){return ((array_p(v))&&(((tag(v))===BYTES)||((tag(v))===MUTABLE_BYTES)));};
function bytes__gInt8Array(bs){return bs[1];};
function make_bytes(k,b){return ((function(t){return (((false||(t===1))===false)?(((false||(t===2))===false)?((function(){return (error("make-bytes","expected one or two arguments"));})()):((function(){return (make_bytes2(k,b));})())):((function(){return (make_bytes1(k));})()));})(arguments.length));};
function make_bytes1(k){var is=new Int8Array(k);return [MUTABLE_BYTES,is];};
function make_bytes2(k,b){var bs=new Int8Array(k);{var i=0,t=k;while((true&&true&&(i<t))){((function(i_17){(bs[i_17]=b);return (i+=1);})(i));};undefined;}return [MUTABLE_BYTES,bs];};
function bytes(){var args=arguments,n=args.length,is=(Int8Array(n));(is.set(args));return [MUTABLE_BYTES,is];};
function bytes__gimmutable_bytes(bs){return [BYTES,(Int8Array.from(bs[1]))];};
function immutable_bytes__gbytes(bs){return [MUTABLE_BYTES,(Int8Array.from(bs[1]))];};
function byte_p(v){return ((exact_integer_p(v))&&(0<=v<=255));};
function bytes_length(bs){return bs[1].length;};
function bytes_ref(bs,k){return bs[1][k];};
function bytes_set_e(bs,k,b){var is=bs[1];return (is[k]=b);};
function subbytes(bs,start,end){return ((function(t){return (((false||(t===2))===false)?(((false||(t===3))===false)?((function(){return (error("subbytes","expected two or three arguments"));})()):((function(){return (subbytes3(bs,start,end));})())):((function(){return (subbytes2(bs,start));})()));})(arguments.length));};
function subbytes2(bs,start){var is=bs[1],end=is.length,k=(end-start),t=new Int8Array(k);{var i=start,t_18=end;while((true&&true&&(i<t_18))){((function(i_19){(t[i_19]=is[i_19]);return (i+=1);})(i));};undefined;}return [MUTABLE_BYTES,is];};
function subbytes3(bs,start,end){var is=bs[1],k=(end-start),t=new Int8Array(k);{var i=start,t_20=end;while((true&&true&&(i<t_20))){((function(i_21){(t[i_21]=is[i_21]);return (i+=1);})(i));};undefined;}return [MUTABLE_BYTES,is];};
function bytes_copy(bs){return (subbytes2(bs,0));};
function bytes_eq_p(bs1,bs2){var n1=(bytes_length(bs1)),n2=(bytes_length(bs2)),is1=bs1[1],is2=bs2[1];return ((n1===n2)&&((function(b){{var i=0,t=n1,cont=true;while((true&&cont&&(i<t))){((function(i_22){skip:while(true){(b=(b&&(is1[i_22]===is2[i_22])));if(!((!b)===false)){(cont=false);break skip;}else ;;(i+=1);break skip;};;return undefined;})(i));};undefined;}return b;})(true)));};
function char_p(v){return ((array_p(v))&&((tag(v))===CHAR));};
function make_char(prim_js_str){return [CHAR,prim_js_str];};
function char__ginteger(c){var s=c[1];return (s.charCodeAt(0));};
function integer__gchar(i){return (String((String.fromCharCode(i))));};
function char_eq_p(c1,c2){return (c1[1]===c2[1]);};
function symbol_p(v){return ((array_p(v))&&((tag(v))===SYMBOL));};
function symbol_interned_p(sym){return ((typeof(sym))==="string");};
function symbol__gstring(sym){var js_str=sym[1],n=js_str.length,a=(Array((n+1)));(a[0]=MUTABLE_STRING);{var i=0,t=n;while((true&&true&&(i<t))){((function(i_23){(a[(i_23+1)]=js_str[i_23]);return (i+=1);})(i));};undefined;}return a;};
function symbol__gimmutable_string(sym){return (String(sym[1]));};
function string__gsymbol(str_24){var t=(typeof(str_24)),r;if(!((t==="string")===false))((function(){return (r=[SYMBOL,str_24]);})());else if(!((t==="object")===false))((function(){var n_a1=t.length,n=(n_a1-1),a=(Array(n));{var i=0,t_25=n;while((true&&true&&(i<t_25))){((function(i_26){(a[i_26]=str_24[(i_26+1)]);return (i+=1);})(i));};undefined;}return (r=[SYMBOL,(String((a.join(""))))]);})());else if(!(true===false))((function(){return (error("string->symbol","expected a string"));})());else undefined;;;;return r;};
function string__guninterned_symbol(str_27){return [SYMBOL,new String(str_27)];};
var gensym_counter=0;
function gensym(base){return ((function(t){return (((false||(t===0))===false)?(((false||(t===1))===false)?((function(){return (error("gensym","expected at most one argument"));})()):((function(){return (gensym1(base));})())):((function(){return (gensym0());})()));})(arguments.length));};
function gensym0(){(gensym_counter+=1);return [SYMBOL,new String(("g"+gensym_counter))];};
function gensym1(base){(console.log(("gensym1: "+base)));(gensym_counter+=1);return [SYMBOL,new String((base+gensym_counter))];};
function symbol_l_p(a_sym,b_sym){return (string_l_p((symbol__gstring(a_sym)),(symbol__gstring(b_sym))));};
function error(who,msg){return (console.log(("error: "+who+": "+msg)));};
function keyword_p(v){return ((array_p(v))&&((tag(v))===KEYWORD));};
function keyword__gstring(key){return (immutable_string__gstring(key[1]));};
function string__gkeyword(str_28){var istr=(string__gimmutable_string(str_28));return [KEYWORD,istr];};
function keyword_l_p(key1,key2){return (key1[1]<key1[2]);};
function pair_p(v){return ((array_p(v))&&((tag(v))===PAIR));};
function null_p(v){return (v===NULL);};
function cons(a,d){return [PAIR,(list_p(d)),a,d];};
function unsafe_car(p){return p[2];};
var car=unsafe_car;
function unsafe_cdr(p){return p[3];};
var cdr=unsafe_cdr;
var Null=NULL;
function list_p(v){return ((v===NULL)||((array_p(v))&&((tag(v))===PAIR)&&v[1]));};
function list(){var args=arguments,n=args.length,n_1=(n-1),xs=NULL;{var i=0,t=n;while((true&&true&&(i<t))){((function(i_29){(xs=(cons(args[(n_1-i_29)],xs)));return (i+=1);})(i));};undefined;}return xs;};
function list_m(){var args=arguments,n=args.length;return (((n===0)===false)?((true===false)?undefined:((function(){var n_2=(n-2),xs=args[(n_2+1)];{var i=0,t=(n-1);while((true&&true&&(i<t))){((function(i_30){(xs=(cons(args[(n_2-i_30)],xs)));return (i+=1);})(i));};undefined;}return xs;})())):((function(){return (error("list*","expected one or more arguments"));})()));};
function build_list(n,proc){var a=(Array(n));{var i=0,t=n;while((true&&true&&(i<t))){((function(i_31){(a[i_31]=(proc(i_31)));return (i+=1);})(i));};undefined;}return (array__glist(a));};
function length(xs){var n=0;while((pair_p(xs))){(n+=1);(xs=(cdr(xs)));};return n;};
function list_ref(xs,i){var ret;if(!((i===0)===false))((function(){return (ret=(car(xs)));})());else if(!((i===1)===false))((function(){return (ret=(car((cdr(xs)))));})());else if(!(true===false))((function(){{var i_32=0,t=(i-1);while((true&&true&&(i_32<t))){((function(j){(xs=(cdr(xs)));return (i_32+=1);})(i_32));};undefined;}return (ret=(car(xs)));})());else undefined;;;;return ret;};
function list_tail(xs,pos){while((!(pos===0))){(xs=(cdr(xs)));(pos-=1);};return xs;};
function append(){var args=arguments,n=args.length;return ((function(t){return (((false||(t===0))===false)?(((false||(t===1))===false)?(((false||(t===2))===false)?((function(){var ret=args[(n-1)],i=(n-2);while((i>=0)){(ret=(append2(args[i],ret)));(i-=1);};return ret;})()):((function(){return (append2(args[0],args[1]));})())):((function(){return args[0];})())):((function(){return NULL;})()));})(n));};
function append2(xs,ys){var ret=ys;if(!((!(null_p(xs)))===false)){var axs=(list__garray(xs)),n=axs.length,n_1=(n-1);{var i=0,t=n;while((true&&true&&(i<t))){((function(i_33){(ret=(cons(axs[(n_1-i_33)],ret)));return (i+=1);})(i));};undefined;}}else ;;return ret;};
function reverse(xs){var result=NULL;while((!(null_p(xs)))){(result=(cons((car(xs)),result)));(xs=(cdr(xs)));};return result;};
function map(proc,xs,ys,zs){return ((function(t){return (((false||(t===2))===false)?(((false||(t===3))===false)?(((false||(t===4))===false)?(((false||(t===0))===false)?((function(){return (1/0);})()):((function(){return (error("map","expected at least two arguments"));})())):((function(){return ((function(xs_39){{var xs_40=xs,xs_41=ys,xs_42=zs;while((true&&true&&(!(null_p(xs_40)))&&(!(null_p(xs_41)))&&(!(null_p(xs_42))))){((function(x,y,z){(xs_39=(cons((proc.call(false,x,y,z)),xs_39)));(xs_40=(cdr(xs_40)));(xs_41=(cdr(xs_41)));return (xs_42=(cdr(xs_42)));})((car(xs_40)),(car(xs_41)),(car(xs_42))));};undefined;}return (reverse(xs_39));})(NULL));})())):((function(){return ((function(xs_36){{var xs_37=xs,xs_38=ys;while((true&&true&&(!(null_p(xs_37)))&&(!(null_p(xs_38))))){((function(x,y){(xs_36=(cons((proc.call(false,x,y)),xs_36)));(xs_37=(cdr(xs_37)));return (xs_38=(cdr(xs_38)));})((car(xs_37)),(car(xs_38))));};undefined;}return (reverse(xs_36));})(NULL));})())):((function(){return ((function(xs_34){{var xs_35=xs;while((true&&true&&(!(null_p(xs_35))))){((function(x){(xs_34=(cons((proc.call(false,x)),xs_34)));return (xs_35=(cdr(xs_35)));})((car(xs_35))));};undefined;}return (reverse(xs_34));})(NULL));})()));})(arguments.length));};
function list__garray(xs){var a,n=(length(xs));(a=(Array(n)));{var xs_43=xs,i=0;while((true&&true&&(!(null_p(xs_43))))){((function(x){(a[i]=x);(xs_43=(cdr(xs_43)));return (i+=1);})((car(xs_43))));};undefined;}return a;};
function array__glist(axs){var n=axs.length,n_1=(n-1),xs=NULL;{var i=0,t=n;while((true&&true&&(i<t))){((function(i_44){(xs=(cons(axs[(n_1-i_44)],xs)));return (i+=1);})(i));};undefined;}return xs;};
function make_list(k,v){return ((function(xs){{var i=0,t=k;while((true&&true&&(i<t))){((function(i_45){(xs=(cons(v,xs)));return (i+=1);})(i));};undefined;}return (reverse(xs));})(NULL));};
function vector_p(v){return ((array_p(v))&&(((tag(v))===VECTOR)||((tag(v))===IMMUTABLE_VECTOR)));};
function make_vector(size,v){return ((function(t){return (((false||(t===1))===false)?(((false||(t===2))===false)?((function(){return (error("make-vector","expected one or two arguments"));})()):((function(){return (make_vector2(size,v));})())):((function(){return (make_vector2(size,0));})()));})(arguments.length));};
function make_vector2(size,v){var a=(Array((size+1)));(a[0]=VECTOR);return ((function(){var i=1,t=(size+1);while((true&&true&&(i<t))){((function(i_46){(a[i_46]=v);return (i+=1);})(i));};undefined;return undefined;})());};
function vector(){var args=arguments,n=args.length,a=(Array((n+1)));(a[0]=VECTOR);{var i=0,t=n;while((true&&true&&(i<t))){((function(j){(a[(j+1)]=args[j]);return (i+=1);})(i));};undefined;}return a;};
function vector_immutable(){var args=arguments,n=args.length,a=(Array((n+1)));(a[0]=IMMUTABLE_VECTOR);{var i=0,t=n;while((true&&true&&(i<t))){((function(j){(a[(j+1)]=args[j]);return (i+=1);})(i));};undefined;}return a;};
function vector_length(v){return (v.length-1);};
function vector_ref(v,i){return v[(i+1)];};
function vector_set_e(vec,i,v){return (vec[(i+1)]=v);};
function vector__glist(vec){var n=vec.length,xs=NULL;{var i=1,t=n;while((true&&true&&(i<t))){((function(i_47){(xs=(cons(vec[(n-i_47)],xs)));return (i+=1);})(i));};undefined;}return xs;};
function list__gvector(vs){var n=(length(vs)),vec=(Array((n+1))),i=1;(vec[0]=VECTOR);while((!(null_p(vs)))){(vec[i]=(car(vs)));(i+=1);(vs=(cdr(vs)));};return vec;};
function vector__gimmutable_vector(vec){var n=vec.length,a=(Array(n));(a[0]=IMMUTABLE_VECTOR);{var i=1,t=n;while((true&&true&&(i<t))){((function(j){(a[j]=vec[j]);return (i+=1);})(i));};undefined;}return vec;};
function vector_fill_e(vec,v){var n=vec.length;{var i=1,t=n;while((true&&true&&(i<t))){((function(i_48){(vec[i_48]=v);return (i+=1);})(i));};undefined;}return VOID;};
function vector_copy_e(dest,dest_start,src,src_start,src_end){return ((function(t){return (((false||(t===3))===false)?(((false||(t===4))===false)?(((false||(t===5))===false)?((function(){return (error("vector-copy!","expected 3, 4, or, 5 arguments"));})()):((function(){return (vector_copy_e5(dest,dest_start,src,src_start,src_end));})())):((function(){return (vector_copy_e5(dest,dest_start,src,src_start,(vector_length(src))));})())):((function(){return (vector_copy_e5(dest,dest_start,src,0,(vector_length(src))));})()));})(arguments.length));};
function vector_copy_e5(dest,dest_start,src,src_start,src_end){{var i=dest_start,i_49=src_start,t=src_end;while((true&&true&&true&&(i_49<t))){((function(i_50,j){(dest[i_50]=src[j]);(i+=1);return (i_49+=1);})(i,i_49));};undefined;}return VOID;};
function build_vector(n,proc){var a=(Array((n+1)));(a[0]=VECTOR);{var i=0,t=n;while((true&&true&&(i<t))){((function(i_51){(a[(i_51+1)]=(proc(i_51)));return (i+=1);})(i));};undefined;}return a;};
function procedure_p(v){return ((typeof(v))==="function");};
function apply(proc,xs){var n=arguments.length;return ((function(t){return (((false||(t===0)||(t===1))===false)?(((false||(t===2))===false)?(((false||(t===3))===false)?(((false||(t===4))===false)?(((false||(t===5))===false)?(((false||(t===6))===false)?undefined:((function(){return (apply3(proc,((function(xs_52){{var i=1,t_53=(n-2);while((true&&true&&(i<t_53))){((function(i_54){(xs_52=(cons(arguments[i_54],xs_52)));return (i+=1);})(i));};undefined;}return (reverse(xs_52));})(NULL)),xs));})())):((function(){return (apply3(proc,(cons(arguments[1],(cons(arguments[2],(cons(arguments[3],NULL)))))),xs));})())):((function(){return (apply3(proc,(cons(arguments[1],(cons(arguments[2],NULL)))),xs));})())):((function(){return (apply3(proc,(cons(arguments[1],NULL)),xs));})())):((function(){return (apply3(proc,NULL,xs));})())):((function(){return (error("procedure?","expected two or more arguments"));})()));})(n));};
function apply3(proc,vs,xs){var nvs=(length(vs)),nxs=(length(xs)),n=(nvs+nxs),a=(Array(n));{var i=0,t=nvs,xs_56=vs;while((true&&true&&(i<t)&&(!(null_p(xs_56))))){((function(i_59,v){(a[i_59]=v);(i+=1);return (xs_56=(cdr(xs_56)));})(i,(car(xs_56))));};undefined;}{var i_55=nvs,t_58=n,xs_57=xs;while((true&&true&&(i_55<t_58)&&(!(null_p(xs_57))))){((function(i_60,x){(a[i_60]=x);(i_55+=1);return (xs_57=(cdr(xs_57)));})(i_55,(car(xs_57))));};undefined;}return (proc.apply(false,a));};
function void_p(v){return (v===VOID);};
function Void(){return VOID;};
function str_null(){return "()";};
function str_number(v){return (v.toString());};
function str_string(v){return ("\""+v+"\"");};
function str_list(v){var a=((function(a){{var xs=v;while((true&&true&&(!(null_p(xs))))){((function(x){(a.push((str(x))));return (xs=(cdr(xs)));})((car(xs))));};undefined;}return a;})([]));return ("("+(a.join(" "))+")");};
function str_vector(v){var a=((function(a){{var xs=v,n=(vector_length(xs)),i=0;while((true&&true&&(i<n))){((function(x){(a.push((str(x))));return (i+=1);})(xs[(i+1)]));};undefined;}return a;})([]));return ("#("+(a.join(" "))+")");};
function str_pair(v){return ("("+(str((car(v))))+" . "+(str((cdr(v))))+")");};
function str_boolean(v){return (((v===true)===false)?(((v===false)===false)?((true===false)?undefined:((function(){return "str -internal error";})())):((function(){return "#f";})())):((function(){return "#t";})()));};
function str_symbol(v){return (string_append("'",(symbol__gstring(v))));};
function str_keyword(v){return ("#:"+v[1]);};
function str_void(v){return "#<void>";};
function str(v){return (((null_p(v))===false)?(((string_p(v))===false)?(((number_p(v))===false)?(((list_p(v))===false)?(((pair_p(v))===false)?(((vector_p(v))===false)?(((boolean_p(v))===false)?(((symbol_p(v))===false)?(((keyword_p(v))===false)?(((void_p(v))===false)?((true===false)?undefined:((function(){(console.log(v));return "str - internal error";})())):((function(){return (str_void(v));})())):((function(){return (str_keyword(v));})())):((function(){return (str_symbol(v));})())):((function(){return (str_boolean(v));})())):((function(){return (str_vector(v));})())):((function(){return (str_pair(v));})())):((function(){return (str_list(v));})())):((function(){return (str_number(v));})())):((function(){return (str_string(v));})())):((function(){return (str_null());})()));};
console.log((string__gsymbol("foo")));
console.log((string((make_char("a")),(make_char("b")),(make_char("c")))));
console.log((string__gsymbol((string((make_char("a")),(make_char("b")),(make_char("c")))))));
console.log((make_primitive_string(0,"a")));
console.log((make_primitive_string(1,"a")));
console.log((make_primitive_string(2,"a")));
console.log((make_primitive_string(3,"a")));
console.log((make_primitive_string(4,"a")));
console.log((make_primitive_string(5,"a")));
console.log((make_primitive_string(6,"a")));
console.log((make_primitive_string(7,"a")));
console.log((typeof((make_primitive_string(7,"a")))));
var as=["a","b"];
console.log((as.join("")));
console.log((string__gsymbol("foo")));
console.log((symbol_p((string__gsymbol("foo")))));
console.log((str((string__gsymbol("foo")))));
console.log((typeof((string__gsymbol("foo")))));
console.log((typeof([3,4,5])));
console.log((symbol__gstring((string__gsymbol("foo")))));
console.log((symbol__gimmutable_string((string__gsymbol("foo")))));
console.log((gensym()));
console.log((gensym()));
console.log((gensym("foo")));
console.log((symbol_l_p("foo","bar")));
console.log((symbol_l_p("bar","foo")));
console.log((string_eq_p("foo","bar")));
console.log((string_eq_p("foo","foo")));
console.log((string_eq_p("foo",(immutable_string__gstring("foo")))));
console.log((string_eq_p("1","1.0")));
console.log((string_upcase("foo")));
console.log((string_upcase((immutable_string__gstring("foo")))));
console.log((str((vector__glist((vector("a","b","c")))))));
console.log((str((list__gvector((vector__glist((vector("a","b","c")))))))));
console.log((str((vector__gimmutable_vector((vector("a","b","c")))))));
console.log((str(((function(v){(vector_fill_e(v,4));return v;})((vector(1,2,3)))))));
console.log((str((build_vector(5,(function(x){return (x+1);}))))));
console.log((str((string__gkeyword("foo")))));
console.log((str((apply(vector,(string__glist("abc")))))));
console.log((str((list(1,2,3)))));
console.log((str((list_tail((list(1,2,3,4,5,6,7,8)),3)))));
console.log((str(((function(xs){{var i=0,t=9;while((true&&true&&(i<t))){((function(i_61){(xs=(cons(i_61,xs)));return (i+=1);})(i));};undefined;}return (reverse(xs));})(NULL)))));
console.log((str((list_m(1,2,3,(list(4,5)))))));
console.log((str((append()))));
console.log((str((append((list(1,2,3)))))));
console.log((str((append((list(1,2,3)),(list(4,5,6)))))));
console.log((str((append((list(1,2,3)),(list(4,5,6)),(list(7,8,9)))))));
console.log((str((append((list(1,2,3)),(list(4,5,6)),(list(7,8,9)),(list(10,11,12)))))));
console.log((str((reverse((list()))))));
console.log((str((reverse((list(1)))))));
console.log((str((reverse((list(1,2)))))));
console.log((str((reverse((list(1,2,3)))))));
console.log((str((reverse((list(1,2,3,4)))))));
console.log((str((map((function(x){return (x+2);}),(list(1,2,3,4)))))));
console.log((str((map((function(x,y){return (x+y);}),(list(1,2,3,4)),(list(11,12,13,14)))))));
