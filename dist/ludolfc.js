!function(e,s){"object"==typeof exports&&"object"==typeof module?module.exports=s():"function"==typeof define&&define.amd?define([],s):"object"==typeof exports?exports.ludolfc=s():e.ludolfc=s()}(this,(()=>{return e={167:(e,s,t)=>{const{Types:r,Errors:i,InterpretError:n,Void:o}=t(842);class a{constructor(e={}){this.variables=[new Map],this.imports=e}clear(){this.variables=[new Map],this.imports&&Object.entries(this.imports).forEach((([e,s])=>this.variables[0].set(e,s)))}hasVariable(e){for(let s=this.variables.length-1;s>=0;s--){if(this.variables[s].has(e))return!0;if(this.variables[s].has("$")&&this.variables[s].get("$").hasAttribute(e))return!0}return!1}getVariable(e){for(let s=this.variables.length-1;s>=0;s--){if(this.variables[s].has(e))return this.variables[s].get(e);if(this.variables[s].has("$")){const t=this.variables[s].get("$");if(t.hasAttribute(e))return t.attribute(e)}}return!1}setVariable(e,s,t=!1){if(t)return void this.variables[this.variables.length-1].set(e,s);let r=!1;for(let t=this.variables.length-1;t>=0;t--)if(this.variables[t].has(e)){this.variables[t].set(e,s),r=!0;break}r||this.variables[this.variables.length-1].set(e,s)}pushScope(){this.variables.push(new Map)}popScope(){this.variables.pop()}}e.exports=class{constructor(e={}){this.variables=new a(e),this.steps=0,this.maxSteps=1e5}execute(e){return this.variables.clear(),this.steps=0,this.executeBlock(e,!1)}executeBlock(e,s=!0){let t;s&&this.variables.pushScope();for(let s of e.statements)t=this.executeStatement(s);return s&&this.variables.popScope(),t||new o}executeStatement(e){return this._stepper(e.source),e.isExpression?this.executeExpression(e):e.isAssignment?this.executeAssignemt(e):e.isWhile?this.executeWhile(e):e.isIf?this.executeIf(e):e}executeExpression(e,s=null){if(this._stepper(e.source),!e.parts)throw new n(i.EMPTY_EXPRESSION,e.source);let t=[...e.parts];return this.executeExpressionParts(t,s)}executeExpressionParts(e,s=null){let t=u("&");if(t){const o=this.executeExpressionParts(e.slice(0,t),s);if(o.type!==r.BOOLEAN)throw new n(i.WRONG_BI_OPERATOR_SUBJECTS,o.source);if(!o.value)return o;const a=this.executeExpressionParts(e.slice(t+1),s);if(a.type!==r.BOOLEAN)throw new n(i.WRONG_BI_OPERATOR_SUBJECTS,a.source);return a}if(t=u("|"),t){const o=this.executeExpressionParts(e.slice(0,t),s);if(o.type!==r.BOOLEAN)throw new n(i.WRONG_BI_OPERATOR_SUBJECTS,o.source);if(o.value)return o;const a=this.executeExpressionParts(e.slice(t+1),s);if(a.type!==r.BOOLEAN)throw new n(i.WRONG_BI_OPERATOR_SUBJECTS,a.source);return a}let o=!1;for(;(t=a())>-1;){const a=e[t];if(s&&!a.isObjectAccess&&!a.isArrayAccess)throw new n(i.ACCESS_OPERATOR_EXPECTED,a.source);if(a.isUni){const s=this.executeExpressionPart(e[t+1]);if(!s.type)throw new n(i.WRONG_UNI_OPERATOR_SUBJECT,s.source);e[t]=a.apply(s),e=h(e,t+1)}else if(a.isBi){const s=this.executeExpressionPart(e[t-1]),r=this.executeExpressionPart(e[t+1]);if(!s.type||!r.type)throw new n(i.WRONG_BI_OPERATOR_SUBJECTS,r.source);if(s.type!==r.type)throw new n(i.UNMATCHING_BI_OPERATOR_SUBJECTS,r.source);e[t]=a.apply(s,r),e=h(e,t-1,t+1)}else if(a.isArrayAccess){const u=this.executeExpressionPart(e[t-1]);if(r.ARRAY!==u.type)throw new n(i.EXPECTED_ARRAY,u.source);const E=a.indexes.map((e=>this.executeExpressionPart(e)));e[t]=a.apply(u,E,s&&c()?s:null),e=h(e,t-1),o=!0}else if(a.isObjectAccess){const r=this.executeExpressionPart(e[t-1]);if(!r.isObject)throw new n(i.EXPECTED_OBJECT,r.source);e[t]=a.apply(r,s&&c()?s:null),e=h(e,t-1),o=!0}else{if(!a.isCall)throw new n(i.UNKNOWN_OPERATOR,a.source);{const s=this.executeExpressionPart(e[t-1]),r=a.params.map((e=>this.executeExpressionPart(e)));e[t]=this.executeFunctionCall(s,r),e=h(e,t-1)}}}if(s&&!o)throw new n(i.ACCESS_OPERATOR_EXPECTED,e[0].source);return this.executeExpressionPart(e[0]);function a(){let s=-1,t=Number.MIN_SAFE_INTEGER;for(let r=0;r<e.length;r++){const i=e[r];i.isOperator&&(t<i.precedence||i.isUni&&t===i.precedence)&&(s=r,t=i.precedence)}return s}function h(e,...s){return e.filter(((e,t)=>!s.includes(t)))}function c(){return 2===e.length}function u(s){for(let t=0;t<e.length;t++)if(e[t].isBi&&e[t].op===s)return t}}executeExpressionPart(e){if(this._stepper(e.source),e.isReference){if(!this.variables.hasVariable(e.varName))throw new n(i.UNREFERENCED_VARIABLE,e.source,e.varName);return this.variables.getVariable(e.varName)}if(e.isExpression)return this.executeExpression(e);if(r.ARRAY===e.type){const s=e.value;for(let e=0;e<s.length;e++)s[e]=this.executeExpressionPart(s[e])}else if(r.OBJECT===e.type){const s=e.value;for(let t of Object.keys(s))s[t]=this.executeExpressionPart(s[t]),(s[t].isObject||s[t].isFunction)&&(s[t].parent=e)}return e}executeFunctionCall(e,s){if(e.isNative)return e.call(...s);if(!s&&e.args||s.length!==e.args.length)throw new n(i.FUNC_ARGUMENTS_MISHMASH,e.source);let t=0;this.variables.pushScope();for(let r of e.args)this.variables.setVariable(r,s[t++],!0);e.parent&&this.variables.setVariable("$",e.parent,!0);try{return this.executeBlock(e.body,!1)}finally{this.variables.popScope()}}executeAssignemt(e){if(!e.left||!e.right)throw new n(i.WRONG_ASSIGNMENT,e.source);const s=this.executeExpressionPart(e.right);if(e.left.isVariable)this.variables.setVariable(e.left.name,s);else{if(!e.left.isExpression)throw new n(i.WRONG_ASSIGNEE_TYPE);this.executeExpression(e.left,s)}}executeWhile(e){if(!e.condition||!e.condition.isExpression)throw new n(i.WRONG_CONDITION,e.source);for(;;){const s=this.executeExpressionPart(e.condition);if(s.type!==r.BOOLEAN)throw new n(i.WRONG_CONDITION_VALUE,s.source);if(!s.value)break;this.executeBlock(e.body)}}executeIf(e){if(!e.condition||!e.condition.isExpression)throw new n(i.WRONG_CONDITION,e.source);const s=this.executeExpressionPart(e.condition);if(s.type!==r.BOOLEAN)throw new n(i.WRONG_CONDITION_VALUE,s.source);s.value?this.executeBlock(e.body):e.elseBody&&this.executeBlock(e.elseBody)}_stepper(e){if(this.steps++,this.steps>this.maxSteps)throw new n(i.EXECUTION_STEPS_EXCEEDED,e)}}},842:e=>{const s=["size","velikost","größe"],t={INVALID_UNI_OPERATOR:"INVALID_UNI_OPERATOR",INVALID_BI_OPERATOR:"INVALID_BI_OPERATOR",UNEXPECTED_END:"UNEXPECTED_END",UNEXPECTED_SYMBOL:"UNEXPECTED_SYMBOL",EXPECTED_SYMBOL:"EXPECTED_SYMBOL",UNREFERENCED_VARIABLE:"UNREFERENCED_VARIABLE",UNEXPEXTED_KEYWORD:"UNEXPEXTED_KEYWORD",INVALID_IDENTIFIER:"INVALID_IDENTIFIER",UNEVEN_OPERATORS:"UNEVEN_OPERATORS",EXPEXTED_FUNCTION:"EXPEXTED_FUNCTION",EXPEXTED_STATEMENT_END:"EXPEXTED_STATEMENT_END",ATTRIBUTE_NOT_EXISTS:"ATTRIBUTE_NOT_EXISTS",ARRAY_INDEX_NOT_NUMBER:"ARRAY_INDEX_NOT_NUMBER",ARRAY_INDEX_MISSING:"ARRAY_INDEX_MISSING",ARRAY_INDEX_OUT_BOUNDS:"ARRAY_INDEX_OUT_BOUNDS",FUNC_ARGUMENTS_MISHMASH:"FUNC_ARGUMENTS_MISHMASH",ATTRIBUTE_ALREADY_EXISTS:"ATTRIBUTE_ALREADY_EXISTS",EMPTY_EXPRESSION:"EMPTY_EXPRESSION",UNKNOWN_OPERATOR:"UNKNOWN_OPERATOR",OPERATOR_NOT_APPLICABLE:"OPERATOR_NOT_APPLICABLE",ACCESS_OPERATOR_EXPECTED:"ACCESS_OPERATOR_EXPECTED",WRONG_UNI_OPERATOR_SUBJECT:"WRONG_UNI_OPERATOR_SUBJECT",WRONG_BI_OPERATOR_SUBJECTS:"WRONG_BI_OPERATOR_SUBJECTS",UNMATCHING_BI_OPERATOR_SUBJECTS:"UNMATCHING_BI_OPERATOR_SUBJECTS",EXPECTED_ARRAY:"EXPECTED_ARRAY",EXPECTED_OBJECT:"EXPECTED_OBJECT",WRONG_ASSIGNMENT:"WRONG_ASSIGNMENT",WRONG_ASSIGNEE_TYPE:"WRONG_ASSIGNEE_TYPE",READONLY_ATTRIBUTE:"READONLY_ATTRIBUTE",WRONG_CONDITION:"WRONG_CONDITION",WRONG_CONDITION_VALUE:"WRONG_CONDITION_VALUE",EXECUTION_STEPS_EXCEEDED:"EXECUTION_STEPS_EXCEEDED",PARSER_STEPS_EXCEEDED:"PARSER_STEPS_EXCEEDED"},r={NUMBER:"NUMBER",BOOLEAN:"BOOLEAN",STRING:"STRING",ARRAY:"ARRAY",OBJECT:"OBJECT",FUNCTION:"FUNCTION",VOID:"VOID"};class i extends Error{constructor(e,s,t,r){super(e),this.message=`${e} ${t?`"${t}"`:""} ${r?`"${r}"`:""}`,this.id=e,this.arg1=t,this.arg2=r,this.position=s,this.isLangError=!0}}class n{constructor(e){this.isExpression=!1,this.isAssignment=!1,this.isWhile=!1,this.isIf=!1,this.source=e}}class o{constructor(e,s=-1){this.op=e,this.isOperator=!0,this.precedence=s}}class a{constructor(e,s,t=r.OBJECT){this.value=e,this.type=t,this.isObject=!0,this.parent=null,this.source=s,this.eq=new p((e=>new E(function(e,s){const t=Object.keys(e.value),r=Object.keys(s.value);if(t.length!==r.length)return!1;for(k of t)if(!e.value[k].eq.call(s.value[k]))return!1;return!0}(this,e)))),this.ne=new p((e=>new E(!this.eq.call(e).value)))}attribute(e,s){const t=this[e]?this[e]:this.value[e];return s&&this.value[e]&&(this.value[e]=s),t||(this.parent?this.parent.attribute(e):void 0)}hasAttribute(e){return this[e]||this.value[e]||this.parent&&this.parent.hasAttribute(e)}}class h extends a{constructor(e,s,t){super(e,s,t),this.eq=new p((e=>new E(this.value===e.value))),this.ne=new p((e=>new E(this.value!==e.value)))}}class c extends h{constructor(e,s){super(e,s,r.NUMBER),this.mult=new p((e=>new c(this.value*e.value))),this.div=new p((e=>new c(this.value/e.value))),this.mod=new p((e=>new c(this.value%e.value))),this.plus=new p((e=>new c(this.value+e.value))),this.minus=new p((e=>new c(this.value-e.value))),this.lt=new p((e=>new E(this.value<e.value))),this.le=new p((e=>new E(this.value<=e.value))),this.gt=new p((e=>new E(this.value>e.value))),this.ge=new p((e=>new E(this.value>=e.value))),this.neg=new p((()=>new c(-this.value))),this.sum=new p(((...e)=>new c(e.reduce(((e,s)=>e+s.value),this.value))))}}class u extends h{constructor(e,t){super(e,t,r.STRING),this.concat=new p((e=>new u(this.value+e.value))),this.length=new p((()=>new c(this.value.length))),this.plus=this.concat;for(let e of s)this[e]=new c(this.value.length)}}class E extends h{constructor(e,s){super(e,s,r.BOOLEAN),this.and=new p((e=>new E(this.value&&e.value))),this.or=new p((e=>new E(this.value||e.value))),this.xor=new p((e=>new E(this.value?!e.value:e.value))),this.nand=new p((e=>new E(!(this.value&&e.value)))),this.neg=new p((()=>new E(!this.value))),this.mult=this.and,this.plus=this.or}}class l extends h{constructor(e,t){super(e,t,r.ARRAY),this.concat=new p((e=>new l(this.value.concat(e.value)))),this.plus=this.concat;for(let e of s)this[e]=new c(this.value.length);this.eq=new p((e=>{if(!e||!e.value)return new E(!1);if(this.value.length!==e.value.length)return new E(!1);for(let s=0;s<this.value.length;s++)if(!this.value[s].eq||!this.value[s].eq.isNative||!this.value[s].eq.call(e.value[s]).value)return new E(!1);return new E(!0)})),this.ne=new p((e=>new E(!this.eq.call(e).value)))}element(e,s){return e.reduce(((r,n,o)=>{const a=Math.ceil(n.value);if(a<0||a>=r.value.length)throw new i(t.ARRAY_INDEX_OUT_BOUNDS);const h=r.value[a];return s&&o===e.length-1&&(r.value[a]=s),h}),this)}attribute(e,r){if(s.includes(e.toLowerCase())){if(r)throw new i(t.READONLY_ATTRIBUTE);return new c(this.value.length)}return super.attribute(e,r)}hasAttribute(e){return s.includes(e.toLowerCase())||super.attribute(e,newValue)}}class p{constructor(e,s){this.type=r.FUNCTION,this.func=e,this.isNative=!0,this.source=s}call(...e){return this.func(...e)}}e.exports={Keywords:{TRUE:["true","pravda","wahr"],FALSE:["false","nepravda","unwahr"],IF:["if","pokud","falls"],ELSE:["else","jinak","sonst"],WHILE:["while","dokud","soweit"]},Errors:t,Types:r,SizeKeywords:s,WhileKeywords:["while","dokud","solange"],IfKeywords:["if","pokud","falls"],ElseKeywords:["else","jinak","sonst"],Block:class{constructor(e,s){this.statements=e,this.source=s}},Assignment:class extends n{constructor(e,s,t){super(t),this.isAssignment=!0,this.left=e,this.right=s}},While:class extends n{constructor(e,s,t){super(t),this.isWhile=!0,this.condition=e,this.body=s}},If:class extends n{constructor(e,s,t,r){super(r),this.isIf=!0,this.condition=e,this.body=s,this.elseBody=t}},Expression:class extends n{constructor(e,s){super(s),this.isExpression=!0,this.parts=e}},Variable:class{constructor(e,s){this.isVariable=!0,this.name=e,this.source=s}},UniOperator:class extends o{constructor(e,s){super(e),this.isUni=!0,this.precedence=this.getPrecedence(),this.source=s}apply(e){const s=function(s){switch(s){case"!":case"-":return e.neg;default:throw new i(t.INVALID_UNI_OPERATOR,this.op)}}(this.op);if(!s||!s.call)throw new i(t.OPERATOR_NOT_APPLICABLE,this.op);return s.call()}getPrecedence(){switch(this.op){case"!":case"-":return 14}}},BiOperator:class extends o{constructor(e,s){super(e),this.isBi=!0,this.precedence=this.getPrecedence(),this.source=s}apply(e,s){const r=function(s){switch(s){case"*":return e.mult;case"/":return e.div;case"%":return e.mod;case"+":return e.plus;case"-":return e.minus;case"<":return e.lt;case"<=":return e.le;case">":return e.gt;case">=":return e.ge;case"=":return e.eq;case"!=":return e.ne;case"&":return e.and;case"|":return e.or;default:throw new i(t.INVALID_BI_OPERATOR,this.op)}}(this.op);if(!r||!r.call){if("="===this.op)return new E(!1);throw new i(t.OPERATOR_NOT_APPLICABLE,this.op)}return r.call(s)}getPrecedence(){switch(this.op){case"*":case"/":case"%":return 12;case"+":case"-":return 11;case"<":case"<=":case">":case">=":return 9;case"=":case"!=":return 8;case"&":return 4;case"|":return 3}}},ArrayAccess:class extends o{constructor(e,s){super("[]",17),this.isAccess=!0,this.isArrayAccess=!0,this.indexes=e,this.source=s}apply(e,s,t){return e.element(s,t)}},ObjectAccess:class extends o{constructor(e,s){super(".",17),this.isAccess=!0,this.isObjectAccess=!0,this.attrName=e,this.source=s}apply(e,s){return e.attribute(this.attrName,s)}},FunctionCall:class extends o{constructor(e,s){super("()",17),this.isAccess=!0,this.isCall=!0,this.params=e,this.source=s}},VarReference:class{constructor(e,s){this.isReference=!0,this.varName=e,this.source=s}},ParseError:class extends i{constructor(e,s,t,r){super(e,s,t,r),this.isParseError=!0}},InterpretError:class extends i{constructor(e,s,t,r){super(e,s,t,r),this.isInterpretError=!0}},Object:a,Number:c,String:u,Boolean:E,Array:l,Function:class{constructor(e,s,t){this.type=r.FUNCTION,this.body=e,this.args=s,this.isFunction=!0,this.source=t,this.eq=new p((e=>new E(!1))),this.ne=new p((e=>new E(!0)))}},NativeFunction:p,Void:class extends h{constructor(e){super(null,e,r.VOID),this.eq=new p((e=>new E(!1))),this.ne=new p((e=>new E(!1)))}}}},17:(e,s,t)=>{const r=t(842),i=t(477),n=t(167);e.exports={LudolfC:class{constructor(e={}){this.parser=new i,this.interpret=new n(e)}execute(e){try{const s=this.parser.parse(e);return this.interpret.execute(s)}catch(s){if(s.isLangError&&(s.position||0===s.position)){const{line:t,col:r}=function(e,s){let t=1,r=1;for(let i=0;i<e.length&&i<=s;i++)r++,"\n"===e[i]&&(t++,r=1);return r=Math.max(1,r-1),{line:t,col:r}}(e,s.position);s.line=t,s.col=r}throw s}}hasVariable(e){return this.interpret.variables.hasVariable(e)}getVariable(e){return this.interpret.variables.getVariable(e)}},lang:r}},477:(e,s,t)=>{const{Errors:r,Keywords:i,WhileKeywords:n,IfKeywords:o,ElseKeywords:a,Block:h,Assignment:c,While:u,If:E,Expression:l,Variable:p,UniOperator:w,BiOperator:f,ArrayAccess:O,ObjectAccess:_,FunctionCall:N,VarReference:T,ParseError:A,Object:P,Number:R,String:b,Boolean:v,Array:d,Function:C,Void:I}=t(842),S=["!","-"],x=["*","/","%","+","-","<","<=",">",">=","=","!=","&","|"],g="ěščřžťďýáíéúůüöäñĚŠČŘŽŤĎÝÁÍÉÚŮÜÖÄÑß",B=`[a-zA-Z_${g}][a-zA-Z0-9_${g}]*`,D=`\\((\\s*(${B})\\s*(,\\s*(${B}))*)?\\s*\\)\\s*{(.|\\s)*\\}`;class m{constructor(e,s=0){this.code=e+"\n",this.pos=0,this.startingAt=s}move(e=1){this.pos+=e}currentChar(){return this.code[this.pos]}remaining(e){return this.code.substring(this.pos,Math.min(e?this.pos+e:this.code.length-1,this.code.length-1))}next(e=1){return this.code.substring(Math.min(this.pos+1,this.code.length-1),Math.min(this.pos+1+e,this.code.length-1))}finished(){return this.pos>=this.code.length}absPos(){return this.pos+this.startingAt-1}}function U(e,s=!1){for(;!e.finished()&&/\s/.test(e.currentChar())&&(!s||"\n"!==e.currentChar());)e.move()}function X(e,s){const t=new RegExp(s);for(;!e.finished()&&!t.test(e.currentChar());)e.move()}function L(e){return e=e.toLowerCase(),Object.values(i).some((s=>s.includes(e)))||n.includes(e)||o.includes(e)}function y(e){return"\n"!==e&&/\s+/g.test(e)}function M(e){return y(e)||Y(e)||"("===e||")"===e||"["===e||"]"===e||"{"===e||"}"===e||"."===e||","===e||x.some((s=>s.startsWith(e)))||S.some((s=>s.startsWith(e)))}function Y(e){return"\n"===e||";"===e}function V(e){return'"'===e||"“"===e||"”"===e||"'"===e}function W(e,s){return s===e||"”"===e&&"“"===s}function G(e){return new RegExp(`^${B}$`).test(e)}function F(e){return n.some((s=>new RegExp(`^\\s*${s}\\s(.*)\\s{`).test(e)))}function k(e){return o.some((s=>new RegExp(`^\\s*${s}\\s(.*)\\s{`).test(e)))}function j(e){return a.some((s=>new RegExp(`^\\s*${s}\\s+{`).test(e)))}e.exports=class{constructor(){this.steps=0,this.maxSteps=1e6}parse(e){return this.steps=0,this.parseBlock(new m(e))}parseBlock(e){const s=[];for(;!e.finished();){const t=this.parseStatement(e);t&&s.push(t)}return new h(s,e.absPos())}parseStatement(e){this._stepper();let s="",t=null,i=!1;const n={arrays:0,objects:0};for(;!e.finished();e.move()){this._stepper();const a=e.currentChar();if(o=e.remaining(),new RegExp("^//").test(o))X(e,"\\n");else{if(!i&&V(a)){let t=a;do{s+=t,e.move(),t=e.currentChar()}while(!e.finished()&&!W(t,a))}if(t||!y(a)||!y(s.charAt(s.length-1))){if(t&&a!==t)throw new A(r.EXPECTED_SYMBOL,e.absPos(),t,a);if("="!==t||a!==t){if("["===a&&n.arrays++,"]"===a&&n.arrays--,"{"===a&&n.objects++,"}"===a&&n.objects--,Y(a)&&!n.arrays&&!n.objects){e.move();break}if(F(e.remaining())){if(s.length)throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),s);U(e),X(e,"\\s");const t=this.parseWhile(e);if(U(e,!0),!Y(e.currentChar()))throw new A(r.EXPEXTED_STATEMENT_END,e.absPos());return t}if(k(e.remaining())){if(s.length)throw new A(r.UNEXPECTED_SYMBOL,s);U(e),X(e,"\\s");const t=this.parseIf(e);if(U(e,!0),j(e.remaining())&&(U(e),X(e,"\\s"),t.elseBody=this.parseBody(e),U(e,!0)),!Y(e.currentChar()))throw new A(r.EXPEXTED_STATEMENT_END,e.absPos());if(!t.elseBody&&(U(e),j(e.remaining())&&(U(e),X(e,"\\s"),t.elseBody=this.parseBody(e),U(e,!0),!Y(e.currentChar()))))throw new A(r.EXPEXTED_STATEMENT_END,e.absPos());return t}if(":"!==a||n.objects){if(i){const t=this.parseExpression(e,n);if(s=s.trim(),G(s)){const r=new p(s,e.absPos());return new c(r,t,e.absPos())}{const i=this.parseExpression(new m(s,e.absPos()-s.length),{});if(!i||i.parts.some((e=>e.isOperator&&!e.isAccess)))throw new A(r.INVALID_IDENTIFIER,e.absPos(),s);return new c(i,t,e.absPos())}}s+=a}else{if(!s.trim().length)throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),a);if(L(s.trim()))throw new A(r.UNEXPEXTED_KEYWORD,e.absPos(),a);t="="}}else i=!0,t=null}}}var o;if(s.length)return this.parseExpression(new m(s,e.absPos()-s.length),{})}parseExpression(e,s,t=null){this._stepper();const i=[];for(;!e.finished();){this._stepper();const c=e.currentChar();if(y(c)){e.move();continue}if(Y(c)||")"===c||"]"===c||"}"===c||","===c){if(!(")"!==c&&"]"!==c&&"}"!==c||(t||t===c)&&i.length))throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),c);if(i.length){if(i[i.length-1].isOperator&&!i[i.length-1].isAccess)throw new A(r.UNEVEN_OPERATORS,e.absPos());return new l(i,e.absPos())}e.move();continue}if(o=e.remaining(),new RegExp(`^${D}`).test(o)){if(i.length&&!i[i.length-1].isOperator)throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),c);const s=this.parseFunction(e);i.push(s);continue}if("{"===c){if(!a())throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),c);e.move();const t=this.readAttributes(e,")");if(U(e),"}"===e.currentChar()){const r=new P(t,e.absPos());for(let e of Object.values(t))e.isObject&&(e.parent=r);i.push(r),e.move(),s.objects--;continue}throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),"}")}if("("===c){if(e.move(),h()){const s=this.readList(e,")");if(U(e),")"!==e.currentChar())throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),")");var n=new N(s,e.absPos());i.push(n),e.move()}else{const t=this.parseExpression(e,s,!0);if(i.push(t),U(e),")"!==e.currentChar())throw new A(r.EXPECTED_SYMBOL,e.absPos(),")");e.move()}continue}if("."===c&&h()){e.move();const s=this.readIdentifier(e);i.push(new _(s,e.absPos()));continue}if("["===c){if(e.move(),h()){const s=this.readList(e,"]");if(U(e),!s.length)throw new A(r.ARRAY_INDEX_MISSING,e.absPos());if("]"!==e.currentChar())throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),"]");if(s.some((e=>!e.isExpression||!e.parts.length)))throw new A(r.ARRAY_INDEX_NOT_NUMBER,e.absPos());e.move(),i.push(new O(s,e.absPos()))}else{const t=this.readList(e,"]");if(U(e),"]"!==e.currentChar())throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),"]");i.push(new d(t,e.absPos())),e.move(),s.arrays--}continue}if(a()){if(S.includes(c)){if(i.push(new w(c,e.absPos())),e.move(),y(e.currentChar()))throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar());continue}}else if(h()){const s=e.remaining(2);if(x.includes(s)){i.push(new f(s,e.absPos())),e.move(2);continue}if(x.includes(c)){i.push(new f(c,e.absPos())),e.move();continue}}if(i.length&&(!i[i.length-1].isOperator||i[i.length-1].isAccess))throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar());const u=this.parseMemberExpression(e);i.push(u)}var o;function a(){return!i.length||i[i.length-1].isOperator&&!i[i.length-1].isAccess}function h(){return i.length&&(!i[i.length-1].isOperator||i[i.length-1].isAccess)}}parseMemberExpression(e){this._stepper();let s="";for(;!e.finished();e.move()){this._stepper();const n=e.currentChar();if(M(n)){if("."===n&&/^(0|([1-9][0-9]*))$/.test(s)&&/[0-9]/.test(e.next())){s+=n;continue}if(i.TRUE.includes(s.toLowerCase()))return new v(!0,e.absPos());if(i.FALSE.includes(s.toLowerCase()))return new v(!1,e.absPos());if(t=s,!isNaN(t)&&!isNaN(parseFloat(t)))return new R(s.includes(".")?parseFloat(s):parseInt(s),e.absPos());if(G(s)||"$"===s)return new T(s,e.absPos());throw new A(r.UNREFERENCED_VARIABLE,e.absPos(),s)}if(V(n))return e.move(),new b(this.readString(e,n),e.absPos());s+=n}var t}readList(e,s){if(U(e),s===e.currentChar())return[];{const t=[];do{const r=this.parseExpression(e,{},s);t.push(r),U(e)}while(","===e.currentChar()&&!e.finished());return t}}readAttributes(e){if(U(e),"}"===e.currentChar())return{};{const s={};let t=!0;do{t||e.move(),t=!1,U(e);const i=this.readIdentifier(e);if(s[i])throw new A(r.ATTRIBUTE_ALREADY_EXISTS,e.absPos(),i);if(U(e),":"!==e.currentChar())throw new A(r.EXPECTED_SYMBOL,e.absPos(),":",e.currentChar());e.move();const n=this.parseExpression(e,{},"}");s[i]=n,U(e)}while(","===e.currentChar()&&!e.finished());return s}}parseWhile(e){const s=this.readUntilBodyOpens(e),t=this.parseExpression(new m(s,e.absPos()-s.length),{},null),r=this.parseBody(e);return new u(t,r,e.absPos())}parseIf(e){const s=this.readUntilBodyOpens(e),t=this.parseExpression(new m(s,e.absPos()-s.length),{},null),r=this.parseBody(e);return new E(t,r,null,e.absPos())}parseFunction(e){const s=this.readArguments(e),t=this.parseBody(e);return new C(t,s,e.absPos())}readArguments(e){if(U(e),"("!==e.currentChar())throw new A(r.EXPECTED_SYMBOL,e.absPos(),"(",e.currentChar());e.move(),U(e);const s=[];let t=!0;for(;(","===e.currentChar()||")"!==e.currentChar())&&!e.finished();){t||e.move(),t=!1;const r=this.readIdentifier(e);s.push(r),U(e)}if(")"!==e.currentChar())throw new A(r.EXPECTED_SYMBOL,e.absPos(),")",e.currentChar());return e.move(),s}parseBody(e){if(U(e),"{"!==e.currentChar())throw new A(r.EXPECTED_SYMBOL,e.absPos(),"{",e.currentChar());e.move(),U(e);let s="",t=0;for(;(t||"}"!==e.currentChar())&&!e.finished();){const r=e.currentChar();s+=r,e.move(),"{"===r&&t++,"}"===r&&t--}if("}"!==e.currentChar())throw new A(r.EXPECTED_SYMBOL,e.absPos(),"}",e.currentChar());return e.move(),/^\s*$/.test(s)?new h([new I(e.absPos())],e.absPos()):this.parseBlock(new m(s,e.absPos()-s.length))}readString(e,s){let t="";for(;!e.finished();e.move()){const r=e.currentChar();if(W(r,s))return e.move(),t;t+=r}throw new A(r.UNEXPECTED_END,e.absPos())}readIdentifier(e){let s="";for(;!e.finished();e.move()){const t=e.currentChar();if(!y(t)){if(!new RegExp(`^${B}$`).test(s+t))break;s+=t}}if(s)return s;throw new A(r.EXPECTED_IDENTIFIER,e.absPos())}readUntilBodyOpens(e){let s=0,t=0,i="";for(;!e.finished();e.move()){const n=e.currentChar();if(V(n))i+=this.readString(e,n);else{if("("===n)t++;else if(")"===n)t--;else if("{"===n){if(!s&&!t)break;s++}else if("}"===n&&(s--,s<0))throw new A(r.UNEXPECTED_SYMBOL,e.absPos(),n);i+=n}}if(i)return i}_stepper(){if(this.steps++,this.steps>this.maxSteps)throw new A(r.PARSER_STEPS_EXCEEDED,source.absPos())}}}},s={},function t(r){var i=s[r];if(void 0!==i)return i.exports;var n=s[r]={exports:{}};return e[r](n,n.exports,t),n.exports}(17);var e,s}));