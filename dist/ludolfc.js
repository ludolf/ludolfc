!function(e,t){"object"==typeof exports&&"object"==typeof module?module.exports=t():"function"==typeof define&&define.amd?define([],t):"object"==typeof exports?exports.ludolfc=t():e.ludolfc=t()}(this,(()=>{return e={202:(e,t,s)=>{const{Types:r,Errors:i,Interruptions:n,InterpretError:a,Interrupt:o,Void:h}=s(746);class c{constructor(e={}){this.variables=new Map,this.parent=null,e&&Object.entries(e).forEach((([e,t])=>this.variables.set(e,t)))}hasVariable(e){return!!this.variables.has(e)||!(!this.variables.has("$")||!this.variables.get("$").hasAttribute(e))||!!this.parent&&this.parent.hasVariable(e)}getVariable(e){if(this.variables.has(e))return this.variables.get(e);if(this.variables.has("$")){const t=this.variables.get("$");if(t.hasAttribute(e))return t.attribute(e)}return!!this.parent&&this.parent.getVariable(e)}setVariable(e,t,s=!1){if(s)return void this.variables.set(e,t);let r=!1,i=this;do{i.variables.has(e)&&(i.variables.set(e,t),r=!0),i=i.parent}while(!r&&i);r||this.variables.set(e,t)}pushScope(){const e=new c;return e.parent=this,e}popScope(){const e=this.parent;return this.parent=null,e}copy(){const e=new c(this.imports);return e.variables=this.variables,e.parent=this.parent,e}}class u{constructor(e,t){this.funcExp=e,this.params=t,this.isFunctionExecution=!0}}class E{constructor(e,t){this.maxSteps=e,this.isInterruptedFn=t,this.steps=0}step(e){if(this.isInterruptedFn&&this.isInterruptedFn())throw new o(n.USER_SUSSPEND);if(this.steps++,this.steps>this.maxSteps)throw new a(i.EXECUTION_STEPS_EXCEEDED,e)}reset(e=null){this.steps=0,e&&(this.maxSteps=e)}}e.exports=class{constructor(e={},t,s=1e5){this.imports=e,this.stepper=new E(s,t&&t.isInterrupted)}async execute(e){return this.variables=new c(this.imports),this.stepper.reset(),await this.executeBlock(e,!1)}async executeBlock(e,t=!0){let s;t&&(this.variables=this.variables.pushScope());for(let t of e.statements)s=await this.executeStatement(t);return t&&(this.variables=this.variables.popScope()),s||new h}async executeStatement(e){return this.stepper.step(e.source),e.isExpression?await this.executeExpression(e):e.isAssignment?await this.executeAssignment(e):e.isWhile?await this.executeWhile(e):e.isIf?await this.executeIf(e):e}async executeExpression(e,t=null){if(this.stepper.step(e.source),!e.parts)throw new a(i.EMPTY_EXPRESSION,e.source);let s=[...e.parts];return await this.executeExpressionParts(s,t)}async executeExpressionParts(e,t=null){const s=async(e,t,s,n)=>{const o=w(e);if(o){const e=await this.executeExpressionParts(t.slice(0,o),s);if(e.type!==r.BOOLEAN)throw new a(i.WRONG_BI_OPERATOR_SUBJECTS,e.source);if(n(e.value))return e;const h=await this.executeExpressionParts(t.slice(o+1),s);if(h.type!==r.BOOLEAN)throw new a(i.WRONG_BI_OPERATOR_SUBJECTS,h.source);return h}},n=await s("|",e,t,(e=>e));if(n)return n;const o=await s("&",e,t,(e=>!e));if(o)return o;let h,c=!1;for(;(h=E())>-1;){const s=e[h];if(t&&!s.isObjectAccess&&!s.isArrayAccess)throw new a(i.ACCESS_OPERATOR_EXPECTED,s.source);try{if(s.isUni){const t=await this.executeExpressionPart(e[h+1]);if(!t.type)throw new a(i.WRONG_UNI_OPERATOR_SUBJECT,s.source);e[h]=s.apply(t),e=l(e,h+1)}else if(s.isBi){const t=await this.executeExpressionPart(e[h-1]),n=await this.executeExpressionPart(e[h+1]);if(!t.type||!n.type)throw new a(i.WRONG_BI_OPERATOR_SUBJECTS,s.source);if(!(t.type===n.type||s.isAddition&&r.STRING===t.type&&n.isPrimitive))throw new a(i.UNMATCHING_BI_OPERATOR_SUBJECTS,s.source);e[h]=s.apply(t,n),e=l(e,h-1,h+1)}else if(s.isArrayAccess){const n=await this.executeExpressionPart(e[h-1]);if(r.ARRAY!==n.type)throw new a(i.EXPECTED_ARRAY,s.source);if(t&&n.protectedAttributes())throw new a(i.PROTECTED_FROM_MODIFICATION,s.source);const o=await Promise.all(s.indexes.map((e=>this.executeExpressionPart(e))));if(e[h]=s.apply(n,o,t&&p()?t:null),!e[h])throw new a(i.ATTRIBUTE_NOT_FOUND,s.source);e=l(e,h-1),c=!0}else if(s.isObjectAccess){const r=await this.executeExpressionPart(e[h-1]);if(!r.isObject&&!r.isFunction)throw new a(i.EXPECTED_OBJECT,s.source);if(t&&r.protectedAttributes())throw new a(i.PROTECTED_FROM_MODIFICATION,s.source);if(e[h]=s.apply(r,t&&p()?t:null),!e[h])throw new a(i.ATTRIBUTE_NOT_FOUND,s.source);e=l(e,h-1),c=!0}else{if(!s.isCall)throw new a(i.UNKNOWN_OPERATOR,s.source);e[h]=new u(e[h-1],s.params),e=l(e,h-1)}}catch(e){if(!e.isLangError)throw new a(i.UNKNOWN_ERROR,s.source,e);if(!e.isInterpretError)throw new a(e.id,s.source,e.arg1,e.arg2);throw e}}if(t&&!c)throw new a(i.ACCESS_OPERATOR_EXPECTED,e[0].source);return await this.executeExpressionPart(e[0]);function E(){let t=-1,s=Number.MIN_SAFE_INTEGER;for(let r=0;r<e.length;r++){const i=e[r];i.isOperator&&(s<i.precedence||i.isUni&&s===i.precedence)&&(t=r,s=i.precedence)}return t}function l(e,...t){return e.filter(((e,s)=>!t.includes(s)))}function p(){return 2===e.length}function w(t){for(let s=0;s<e.length;s++)if(e[s].isBi&&e[s].op===t)return s}}async executeExpressionPart(e){if(this.stepper.step(e.source),e.isReference){if(!this.variables.hasVariable(e.varName))throw new a(i.UNREFERENCED_VARIABLE,e.source-e.varName.length,e.varName);return this.variables.getVariable(e.varName)}if(e.isFunctionExecution){const t=await this.executeExpressionPart(e.funcExp),s=await Promise.all(e.params.map((e=>this.executeExpressionPart(e))));return await this.executeFunctionCall(t,s)}if(e.isExpression)return await this.executeExpression(e);if(r.ARRAY===e.type){const t=e.value;for(let e=0;e<t.length;e++)t[e]=await this.executeExpressionPart(t[e])}else if(r.OBJECT===e.type){const t=e.value;for(let s of Object.keys(t))t[s]=await this.executeExpressionPart(t[s]),(t[s].isObject||t[s].isFunction)&&(t[s].parent=e)}return e.isFunction&&!e.scope&&(e.scope=this.variables.copy()),e}async executeFunctionCall(e,t){if(e.isNative){return await e.call(...t)||new h}if(!t&&e.args||t.length!==e.args.length)throw new a(i.FUNC_ARGUMENTS_MISHMASH,e.source);let s=0;const r=this.variables;this.variables=e.scope.pushScope();for(let r of e.args)this.variables.setVariable(r,t[s++],!0);e.parent&&this.variables.setVariable("$",e.parent,!0);try{const t=e.body.copy();return await this.executeBlock(t,!1)}finally{this.variables.popScope(),this.variables=r}}async executeAssignment(e){if(!e.left||!e.right)throw new a(i.WRONG_ASSIGNMENT,e.source);const t=await this.executeExpressionPart(e.right);if(e.left.isVariable){const s=this.variables.getVariable(e.left.name);if(s.protected&&s.protected())throw new a(i.PROTECTED_FROM_MODIFICATION,e.left.source);this.variables.setVariable(e.left.name,t)}else{if(!e.left.isExpression)throw new a(i.WRONG_ASSIGNEE_TYPE);await this.executeExpression(e.left,t)}}async executeWhile(e){if(!e.condition||!e.condition.isExpression)throw new a(i.WRONG_CONDITION,e.source);for(;;){const t=await this.executeExpressionPart(e.condition);if(t.type!==r.BOOLEAN)throw new a(i.WRONG_CONDITION_VALUE,t.source);if(!t.value)break;await this.executeBlock(e.body.copy())}}async executeIf(e){if(!e.condition||!e.condition.isExpression)throw new a(i.WRONG_CONDITION,e.source);const t=await this.executeExpressionPart(e.condition);if(t.type!==r.BOOLEAN)throw new a(i.WRONG_CONDITION_VALUE,t.source);t.value?await this.executeBlock(e.body):e.elseBody&&await this.executeBlock(e.elseBody)}}},746:e=>{const t={SIZE:["size","velikost","größe"]},s={INVALID_UNI_OPERATOR:"INVALID_UNI_OPERATOR",INVALID_BI_OPERATOR:"INVALID_BI_OPERATOR",UNEXPECTED_END:"UNEXPECTED_END",UNEXPECTED_SYMBOL:"UNEXPECTED_SYMBOL",EXPECTED_SYMBOL:"EXPECTED_SYMBOL",UNREFERENCED_VARIABLE:"UNREFERENCED_VARIABLE",UNEXPECTED_KEYWORD:"UNEXPECTED_KEYWORD",INVALID_IDENTIFIER:"INVALID_IDENTIFIER",UNEVEN_OPERATORS:"UNEVEN_OPERATORS",EXPECTED_FUNCTION:"EXPECTED_FUNCTION",EXPECTED_STATEMENT_END:"EXPECTED_STATEMENT_END",ARRAY_INDEX_NOT_NUMBER:"ARRAY_INDEX_NOT_NUMBER",ARRAY_INDEX_MISSING:"ARRAY_INDEX_MISSING",ARRAY_INDEX_OUT_BOUNDS:"ARRAY_INDEX_OUT_BOUNDS",FUNC_ARGUMENTS_MISHMASH:"FUNC_ARGUMENTS_MISHMASH",ATTRIBUTE_ALREADY_EXISTS:"ATTRIBUTE_ALREADY_EXISTS",ATTRIBUTE_NOT_FOUND:"ATTRIBUTE_NOT_FOUND",ELEMENT_NOT_FOUND:"ELEMENT_NOT_FOUND",EMPTY_EXPRESSION:"EMPTY_EXPRESSION",UNKNOWN_OPERATOR:"UNKNOWN_OPERATOR",OPERATOR_NOT_APPLICABLE:"OPERATOR_NOT_APPLICABLE",ACCESS_OPERATOR_EXPECTED:"ACCESS_OPERATOR_EXPECTED",WRONG_UNI_OPERATOR_SUBJECT:"WRONG_UNI_OPERATOR_SUBJECT",WRONG_BI_OPERATOR_SUBJECTS:"WRONG_BI_OPERATOR_SUBJECTS",UNMATCHING_BI_OPERATOR_SUBJECTS:"UNMATCHING_BI_OPERATOR_SUBJECTS",EXPECTED_ARRAY:"EXPECTED_ARRAY",EXPECTED_OBJECT:"EXPECTED_OBJECT",EXPECTED_IDENTIFIER:"EXPECTED_IDENTIFIER",WRONG_ASSIGNMENT:"WRONG_ASSIGNMENT",WRONG_ASSIGNEE_TYPE:"WRONG_ASSIGNEE_TYPE",READONLY_ATTRIBUTE:"READONLY_ATTRIBUTE",WRONG_CONDITION:"WRONG_CONDITION",WRONG_CONDITION_VALUE:"WRONG_CONDITION_VALUE",EXECUTION_STEPS_EXCEEDED:"EXECUTION_STEPS_EXCEEDED",PARSER_STEPS_EXCEEDED:"PARSER_STEPS_EXCEEDED",PROTECTED_FROM_MODIFICATION:"PROTECTED_FROM_MODIFICATION",DIVISION_BY_ZERO:"DIVISION_BY_ZERO",UNKNOWN_ERROR:"UNKNOWN_ERROR"},r={NUMBER:"NUMBER",BOOLEAN:"BOOLEAN",STRING:"STRING",ARRAY:"ARRAY",OBJECT:"OBJECT",FUNCTION:"FUNCTION",VOID:"VOID"};class i extends Error{constructor(e,t,s,r){super(e),this.details=`${s?`"${s}"`:""} ${r?`"${r}"`:""}`,this.message=`${e} ${this.details}`,this.id=e,this.arg1=s,this.arg2=r,this.position=t,this.isLangError=!0}}class n{constructor(e,t){this.statements=e,this.source=t}copy(){return new n(this.statements.map((e=>e.copy())),this.source)}}class a{constructor(e){this.isExpression=!1,this.isAssignment=!1,this.isWhile=!1,this.isIf=!1,this.source=e}copy(){return this}}class o extends a{constructor(e,t,s){super(s),this.isAssignment=!0,this.left=e,this.right=t}copy(){return new o(this.left.copy(),this.right.copy(),this.source)}}class h extends a{constructor(e,t,s){super(s),this.isWhile=!0,this.condition=e,this.body=t}copy(){return new h(this.condition.copy(),this.body.copy(),this.source)}}class c extends a{constructor(e,t,s,r){super(r),this.isIf=!0,this.condition=e,this.body=t,this.elseBody=s}copy(){return new c(this.condition.copy(),this.body.copy(),this.elseBody?.copy(),this.source)}}class u extends a{constructor(e,t){super(t),this.isExpression=!0,this.parts=e}copy(){return new u(this.parts.map((e=>e.copy())),this.source)}}class E{constructor(e,t=-1){this.op=e,this.isOperator=!0,this.precedence=t}copy(){return this}}class l{constructor(e,t,s=r.OBJECT){this.value=e,this.type=s,this.isObject=!0,this.parent=null,this.source=t,this.eq=new T((e=>new O(function(e,t){const s=Object.keys(e.value),r=Object.keys(t.value);if(s.length!==r.length)return!1;for(k of s)if(!e.value[k].eq.call(t.value[k]))return!1;return!0}(this,e)))),this.ne=new T((e=>new O(!this.eq.call(e).value)))}attribute(e,t){const s=this[e]?this[e]:this.value[e];return t&&this.value[e]&&(this.value[e]=t),s||(this.parent?this.parent.attribute(e):void 0)}hasAttribute(e){return this[e]||this.value[e]||this.parent&&this.parent.hasAttribute(e)}protected(){return this.isProtected||this.parent&&this.parent.protected()}protectedAttributes(){return this.protected()}copy(){const e={};return Object.keys(this.value).forEach((t=>e[t]=this.value[t].copy())),new l(e,this.source)}}class p extends l{constructor(e,t,s){super(e,t,s),this.eq=new T((e=>new O(this.value===e.value))),this.ne=new T((e=>new O(this.value!==e.value)))}copy(){return this}}class w extends p{constructor(e,t){super(e,t,r.NUMBER),this.isPrimitive=!0,this.mult=new T((e=>new w(this.value*e.value))),this.div=new T((e=>new w(this.value/e.value))),this.mod=new T((e=>new w(this.value%e.value))),this.plus=new T((e=>new w(this.value+e.value))),this.minus=new T((e=>new w(this.value-e.value))),this.lt=new T((e=>new O(this.value<e.value))),this.le=new T((e=>new O(this.value<=e.value))),this.gt=new T((e=>new O(this.value>e.value))),this.ge=new T((e=>new O(this.value>=e.value))),this.neg=new T((()=>new w(-this.value))),this.sum=new T(((...e)=>new w(e.reduce(((e,t)=>e+t.value),this.value)))),this.round=new T((()=>new w(Math.round(this.value)))),this.floor=new T((()=>new w(Math.floor(this.value)))),this.ceil=new T((()=>new w(Math.ceil(this.value))))}}class f extends p{constructor(e,s){super(e,s,r.STRING),this.isPrimitive=!0,this.concat=new T((e=>new f(this.value+e.value))),this.length=new T((()=>new w(this.value.length))),this.charAt=new T((e=>new f(this.value.charAt(e.value)))),this.sub=new T(((e,t)=>e.value>this.value.length-1||e.value<0||t&&t.value<=e.value||t&&t.value>this.value.length?new f(""):new f(this.value.substring(e.value,t?t.value:this.value.length)))),this.plus=this.concat;for(let e of t.SIZE)this[e]=new w(this.value.length)}}class O extends p{constructor(e,t){super(e,t,r.BOOLEAN),this.isPrimitive=!0,this.and=new T((e=>new O(this.value&&e.value))),this.or=new T((e=>new O(this.value||e.value))),this.xor=new T((e=>new O(this.value?!e.value:e.value))),this.nand=new T((e=>new O(!(this.value&&e.value)))),this.neg=new T((()=>new O(!this.value))),this.mult=this.and,this.plus=this.or}}class N extends p{constructor(e,s){super(e,s,r.ARRAY),this.concat=new T((e=>new N(this.value.concat(e.value)))),this.plus=this.concat;for(let e of t.SIZE)this[e]=new w(this.value.length);this.eq=new T((e=>{if(!e||!e.value)return new O(!1);if(this.value.length!==e.value.length)return new O(!1);for(let t=0;t<this.value.length;t++)if(!this.value[t].eq||!this.value[t].eq.isNative||!this.value[t].eq.call(e.value[t]).value)return new O(!1);return new O(!0)})),this.ne=new T((e=>new O(!this.eq.call(e).value)))}element(e,t){return e.reduce(((r,n,a)=>{const o=Math.ceil(n.value);if(o<0||o>=r.value.length)throw new i(s.ARRAY_INDEX_OUT_BOUNDS,null,o);const h=r.value[o];return t&&a===e.length-1&&(r.value[o]=t),h}),this)}attribute(e,r){if(t.SIZE.includes(e.toLowerCase())){if(r)throw new i(s.READONLY_ATTRIBUTE);return new w(this.value.length)}return super.attribute(e,r)}hasAttribute(e){return t.SIZE.includes(e.toLowerCase())||super.attribute(e,newValue)}copy(){return new N(this.value.map((e=>e.copy())),this.source)}}class _{constructor(e,t,s,i){this.type=r.FUNCTION,this.body=e,this.args=t,this.isFunction=!0,this.source=i,this.funcId=s,this.eq=new T((e=>new O(e.funcId===this.funcId))),this.ne=new T((e=>new O(e.funcId!==this.funcId)))}attribute(e,t){if(t)throw new i(s.READONLY_ATTRIBUTE);const r=this[e]?this[e]:this.value[e];if(r)return r}hasAttribute(e){return this[e]||this.value[e]}protectedAttributes(){return!0}copy(){return new _(this.body,this.args,this.funcId,this.source)}}class T{constructor(e,t){this.type=r.FUNCTION,this.func=e,this.isNative=!0,this.source=t}call(...e){return this.func(...e)}copy(){return this}}e.exports={Keywords:{TRUE:["true","pravda","wahr"],FALSE:["false","nepravda","falsch"],IF:["if","pokud","falls"],ELSE:["else","jinak","sonst"],WHILE:["while","dokud","solange"]},Errors:s,Interruptions:{USER_SUSSPEND:"USER_SUSSPEND"},Types:r,Block:n,Assignment:o,While:h,If:c,Expression:u,Variable:class{constructor(e,t){this.isVariable=!0,this.name=e,this.source=t}copy(){return this}},UniOperator:class extends E{constructor(e,t){super(e),this.isUni=!0,this.precedence=this.getPrecedence(),this.source=t}apply(e){const t=function(t){switch(t){case"!":case"-":return e.neg;default:throw new i(s.INVALID_UNI_OPERATOR,null,this.op)}}(this.op);if(!t||!t.call)throw new i(s.OPERATOR_NOT_APPLICABLE,null,this.op);return t.call()}getPrecedence(){switch(this.op){case"!":case"-":return 14}}},BiOperator:class extends E{constructor(e,t){super(e),this.isBi=!0,this.precedence=this.getPrecedence(),this.isAddition="+"===e,this.source=t}apply(e,t){const n=function(t){switch(t){case"*":return e.mult;case"/":return e.div;case"%":return e.mod;case"+":return e.plus;case"-":return e.minus;case"<":return e.lt;case"<=":return e.le;case">":return e.gt;case">=":return e.ge;case"=":return e.eq;case"!=":return e.ne;case"&":return e.and;case"|":return e.or;default:throw new i(s.INVALID_BI_OPERATOR,null,this.op)}}(this.op);if(!n||!n.call){if("="===this.op)return new O(!1);throw new i(s.OPERATOR_NOT_APPLICABLE,null,this.op)}if("/"===this.op&&r.NUMBER===t.type&&0===t.value)throw new i(s.DIVISION_BY_ZERO,null,this.op);return n.call(t)}getPrecedence(){switch(this.op){case"*":case"/":case"%":return 12;case"+":case"-":return 11;case"<":case"<=":case">":case">=":return 9;case"=":case"!=":return 8;case"&":return 4;case"|":return 3}}},ArrayAccess:class extends E{constructor(e,t){super("[]",17),this.isAccess=!0,this.isArrayAccess=!0,this.indexes=e,this.source=t}apply(e,t,s){return e.element(t,s)}},ObjectAccess:class extends E{constructor(e,t){super(".",17),this.isAccess=!0,this.isObjectAccess=!0,this.attrName=e,this.source=t}apply(e,t){return e.attribute(this.attrName,t)}},FunctionCall:class extends E{constructor(e,t){super("()",17),this.isAccess=!0,this.isCall=!0,this.params=e,this.source=t}},VarReference:class{constructor(e,t){this.isReference=!0,this.varName=e,this.source=t}copy(){return this}},ParseError:class extends i{constructor(e,t,s,r){super(e,t,s,r),this.isParseError=!0}},InterpretError:class extends i{constructor(e,t,s,r){super(e,t,s,r),this.isInterpretError=!0}},Interrupt:class{constructor(e){this.id=e,this.isLangInterruption=!0}},Object:l,Number:w,String:f,Boolean:O,Array:N,Function:_,NativeFunction:T,Void:class extends p{constructor(e){super(null,e,r.VOID),this.eq=new T((e=>new O(!1))),this.ne=new T((e=>new O(!1)))}}}},337:(e,t,s)=>{const r=s(746),i=s(417),n=s(202);e.exports={LudolfC:class{constructor(e={},t={}){this.parser=new i,this.interpret=new n(e,t)}async execute(e){try{const t=this.parser.parse(e);return await this.interpret.execute(t)}catch(e){if(e.isLangError&&(e.position||0===e.position)){const{line:t,col:s}=function(e,t){let s=1,r=1;for(let i=0;i<e.length&&i<=t;i++)r++,"\n"===e.charAt(i)&&(s++,r=1);return r=Math.max(1,r-1),{line:s,col:r}}(this.parser.source.code,e.position);e.line=t,e.col=s}throw e}}hasVariable(e){return this.interpret.variables.hasVariable(e)}getVariable(e){return this.interpret.variables.getVariable(e)}},lang:r}},417:(e,t,s)=>{const{Errors:r,Keywords:i,Block:n,Assignment:a,While:o,If:h,Expression:c,Variable:u,UniOperator:E,BiOperator:l,ArrayAccess:p,ObjectAccess:w,FunctionCall:f,VarReference:O,ParseError:N,Object:_,Number:T,String:R,Boolean:A,Array:I,Function:P,Void:d}=s(746),v=["!","-"],b=["*","/","%","+","-","<","<=",">",">=","=","!=","&","|"],C="ěščřžťďýáíéúůüöäňñĚŠČŘŽŤĎÝÁÍÉÚŮÜÖÄŇÑß",S=`[a-zA-Z_${C}][a-zA-Z0-9_${C}]*`,D=`\\((\\s*(${S})\\s*(,\\s*(${S}))*)?\\s*\\)\\s*{(.|\\s)*\\}`;class g{constructor(e,t=0){this.code=function(e){if(!e.length)return e;let t="",s=null,r=!1,i=!1,n=0;for(;n<e.length-1;n++){const a=e.charAt(n);!r&&M(a)?(r=!0,s=a):r&&X(a,s)?(r=!1,s=null):r||"/"!==a||"/"!==e.charAt(n+1)?"\n"===a&&(i=!1):i=!0,i||(t+=a)}return t+e.charAt(e.length-1)}(e+"\n"),this.pos=0,this.startingAt=t}move(e=1){this.pos+=e}currentChar(){return this.code.charAt(this.pos)}remaining(e=void 0){return this.code.substring(this.pos,Math.min(e?this.pos+e:this.code.length-1,this.code.length-1))}next(e=1){return this.code.substring(Math.min(this.pos+1,this.code.length-1),Math.min(this.pos+1+e,this.code.length-1))}finished(){return this.pos>=this.code.length}absPos(){return this.pos+this.startingAt-1}}function x(e,t=!1){for(;!e.finished()&&/\s/.test(e.currentChar())&&(!t||"\n"!==e.currentChar());)e.move()}function m(e,t){const s=new RegExp(t);for(;!e.finished()&&!s.test(e.currentChar());)e.move()}function B(e){return e=e.toLowerCase(),Object.values(i).some((t=>t.includes(e)))||i.WHILE.includes(e)||i.IF.includes(e)}function U(e,t=!1){return("\n"!==e||t)&&/\s+/g.test(e)}function y(e){return U(e)||L(e)||"("===e||")"===e||"["===e||"]"===e||"{"===e||"}"===e||"."===e||","===e||b.some((t=>t.startsWith(e)))||v.some((t=>t.startsWith(e)))}function L(e){return"\n"===e||";"===e}function M(e){return'"'===e||"“"===e||"”"===e||"'"===e}function X(e,t){return t===e||"”"===e&&"“"===t}function F(e){return new RegExp(`^${S}$`).test(e)}function Y(e){return i.WHILE.some((t=>new RegExp(`^\\s*${t}\\s(.+)\\s{`,"s").test(e)))}function V(e){return i.IF.some((t=>new RegExp(`^\\s*${t}\\s(.+)\\s{`,"s").test(e)))}e.exports=class{constructor(){this.steps=0,this.maxSteps=1e6,this.fcount=0,this.source=new g("")}parse(e){return this.steps=0,this.source=new g(e),this.parseBlock(this.source)}parseBlock(e){const t=[];for(;!e.finished();){const s=this.parseStatement(e);s&&t.push(s)}return new n(t,e.absPos())}parseStatement(e){this._stepper();let t="",s=null,i=!1;const n={arrays:0,objects:0,groups:0,ops:0};for(;!e.finished();e.move()){this._stepper();const o=e.currentChar();if(!i&&M(o)){let s=o;do{t+=s,e.move(),s=e.currentChar()}while(!e.finished()&&!X(s,o))}if(s||!U(o)||!U(t.charAt(t.length-1))){if(s&&o!==s)throw new N(r.EXPECTED_SYMBOL,e.absPos(),s,o);if("="!==s||o!==s){if("["===o&&n.arrays++,"]"===o&&n.arrays--,"{"===o&&n.objects++,"}"===o&&n.objects--,"("===o&&n.groups++,")"===o&&n.groups--,"+"===o||"-"===o||"*"===o||"/"===o||"<"===o||">"===o||"="===o?n.ops++:n.ops&&!U(o,!0)&&n.ops--,L(o)&&!n.arrays&&!n.objects&&!n.groups&&!n.ops){e.move();break}if(Y(e.remaining())){if(t.length)throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),t);x(e),m(e,"\\s");const s=this.parseWhile(e);if(x(e,!0),!L(e.currentChar()))throw new N(r.EXPECTED_STATEMENT_END,e.absPos());return s}if(V(e.remaining())){if(t.length)throw new N(r.UNEXPECTED_SYMBOL,t);return this.parseIfStatement(e)}if(":"!==o||n.objects){if(i){const s=this.parseExpression(e,n);if(t=t.trim(),F(t)){const r=new u(t,e.absPos());return new a(r,s,e.absPos())}{const i=this.parseExpression(new g(t,e.absPos()-t.length),{});if(!i||i.parts.some((e=>e.isOperator&&!e.isAccess)))throw new N(r.INVALID_IDENTIFIER,e.absPos(),t);return new a(i,s,e.absPos())}}t+=o}else{if(!t.trim().length)throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),o);if(B(t.trim()))throw new N(r.UNEXPECTED_KEYWORD,e.absPos(),o);s="="}}else i=!0,s=null}}if(t.length)return this.parseExpression(new g(t,e.absPos()-t.length),{})}parseExpression(e,t,s=null){this._stepper();const i=[];for(;!e.finished();){this._stepper();const u=e.currentChar();if(U(u)){e.move();continue}if(L(u)||")"===u||"]"===u||"}"===u||","===u){if(!(")"!==u&&"]"!==u&&"}"!==u||(s||s===u)&&i.length))throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),u);if(i.length&&(!i[i.length-1].isOperator||i[i.length-1].isAccess))return new c(i,e.absPos());e.move();continue}if(a=e.remaining(),new RegExp(`^${D}`).test(a)){if(i.length&&!i[i.length-1].isOperator)throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),u);const t=this.parseFunction(e);i.push(t);continue}if("{"===u){if(!o())throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),u);e.move();const s=this.readAttributes(e,")");if(x(e),"}"===e.currentChar()){const r=new _(s,e.absPos());for(let e of Object.values(s))e.isObject&&(e.parent=r);i.push(r),e.move(),t.objects--;continue}throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),"}")}if("("===u){if(e.move(),h()){const t=this.readList(e,")");if(x(e),")"!==e.currentChar())throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),")");var n=new f(t,e.absPos());i.push(n),e.move()}else{const s=this.parseExpression(e,t,!0);if(i.push(s),x(e),")"!==e.currentChar())throw new N(r.EXPECTED_SYMBOL,e.absPos(),")");e.move()}continue}if("."===u&&h()){e.move();const t=this.readIdentifier(e);i.push(new w(t,e.absPos()));continue}if("["===u){if(e.move(),h()){const t=this.readList(e,"]");if(x(e),!t.length)throw new N(r.ARRAY_INDEX_MISSING,e.absPos());if("]"!==e.currentChar())throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),"]");if(t.some((e=>!e.isExpression||!e.parts.length)))throw new N(r.ARRAY_INDEX_NOT_NUMBER,e.absPos());e.move(),i.push(new p(t,e.absPos()))}else{const s=this.readList(e,"]");if(x(e),"]"!==e.currentChar())throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar(),"]");i.push(new I(s,e.absPos())),e.move(),t.arrays--}continue}if(o()){if(v.includes(u)){if(i.push(new E(u,e.absPos())),e.move(),U(e.currentChar()))throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar());continue}}else if(h()){const t=e.remaining(2);if(b.includes(t)){i.push(new l(t,e.absPos())),e.move(t.length);continue}if(b.includes(u)){i.push(new l(u,e.absPos())),e.move();continue}}if(h())throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),e.currentChar());const O=this.parseMemberExpression(e);i.push(O)}var a;if(i.length&&i[i.length-1].isOperator&&!i[i.length-1].isAccess)throw new N(r.UNEVEN_OPERATORS,e.absPos()-1);function o(){return!i.length||i[i.length-1].isOperator&&!i[i.length-1].isAccess}function h(){return i.length&&(!i[i.length-1].isOperator||i[i.length-1].isAccess)}}parseMemberExpression(e){this._stepper();let t="";for(;!e.finished();e.move()){this._stepper();const n=e.currentChar();if(y(n)){if("."===n&&/^(0|([1-9][0-9]*))$/.test(t)&&/[0-9]/.test(e.next())){t+=n;continue}if(i.TRUE.includes(t.toLowerCase()))return new A(!0,e.absPos());if(i.FALSE.includes(t.toLowerCase()))return new A(!1,e.absPos());if(s=t,!isNaN(s)&&!isNaN(parseFloat(s)))return new T(t.includes(".")?parseFloat(t):parseInt(t),e.absPos());if(F(t)||"$"===t)return new O(t,e.absPos());throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),t)}if(M(n))return e.move(),new R(this.readString(e,n),e.absPos());t+=n}var s}parseIfStatement(e){x(e),m(e,"\\s");const t=this.parseIf(e);x(e,!0);let s=L(e.currentChar());if(x(e),a=e.remaining(),i.ELSE.some((e=>new RegExp(`^\\s*${e}\\s+{`,"s").test(a))))m(e,"\\s"),t.elseBody=this.parseBody(e),x(e,!0);else if(function(e){return i.ELSE.some((t=>new RegExp(`^\\s*${t}\\s+(${i.IF.join("|")})`).test(e)))}(e.remaining())){m(e,"\\s");const r=this.parseIfStatement(e);t.elseBody=new n([r],e.absPos()),s=!0}var a;if(!s&&!L(e.currentChar()))throw new N(r.EXPECTED_STATEMENT_END,e.absPos());return t}readList(e,t){if(x(e),t===e.currentChar())return[];{const s=[];do{const r=this.parseExpression(e,{},t);s.push(r),x(e)}while(","===e.currentChar()&&!e.finished());return s}}readAttributes(e){if(x(e),"}"===e.currentChar())return{};{const t={};let s=!0;do{s||e.move(),s=!1,x(e);const i=this.readIdentifier(e);if(t[i])throw new N(r.ATTRIBUTE_ALREADY_EXISTS,e.absPos(),i);if(x(e),":"!==e.currentChar())throw new N(r.EXPECTED_SYMBOL,e.absPos(),":",e.currentChar());e.move();const n=this.parseExpression(e,{},"}");t[i]=n,x(e)}while(","===e.currentChar()&&!e.finished());return t}}parseWhile(e){const t=this.readUntilBodyOpens(e),s=this.parseExpression(new g(t,e.absPos()-t.length),{},null),r=this.parseBody(e);return new o(s,r,e.absPos())}parseIf(e){const t=this.readUntilBodyOpens(e),s=this.parseExpression(new g(t,e.absPos()-t.length),{},null),r=this.parseBody(e);return new h(s,r,null,e.absPos())}parseFunction(e){const t=this.readArguments(e),s=this.parseBody(e);return new P(s,t,++this.fcount,e.absPos())}readArguments(e){if(x(e),"("!==e.currentChar())throw new N(r.EXPECTED_SYMBOL,e.absPos(),"(",e.currentChar());e.move(),x(e);const t=[];let s=!0;for(;(","===e.currentChar()||")"!==e.currentChar())&&!e.finished();){s||e.move(),s=!1;const r=this.readIdentifier(e);t.push(r),x(e)}if(")"!==e.currentChar())throw new N(r.EXPECTED_SYMBOL,e.absPos(),")",e.currentChar());return e.move(),t}parseBody(e){if(x(e),"{"!==e.currentChar())throw new N(r.EXPECTED_SYMBOL,e.absPos(),"{",e.currentChar());e.move(),x(e);let t="",s=0;for(;(s||"}"!==e.currentChar())&&!e.finished();){const r=e.currentChar();t+=r,e.move(),"{"===r&&s++,"}"===r&&s--}if("}"!==e.currentChar())throw new N(r.EXPECTED_SYMBOL,e.absPos(),"}",e.currentChar());return e.move(),/^\s*$/.test(t)?new n([new d(e.absPos())],e.absPos()):this.parseBlock(new g(t,e.absPos()-t.length))}readString(e,t){let s="";for(;!e.finished();e.move()){const r=e.currentChar();if(X(r,t))return e.move(),s;s+=r}throw new N(r.UNEXPECTED_END,e.absPos())}readIdentifier(e){let t="";for(;!e.finished();e.move()){const s=e.currentChar();if(!U(s)){if(!new RegExp(`^${S}$`).test(t+s))break;t+=s}}if(t)return t;throw new N(r.EXPECTED_IDENTIFIER,e.absPos())}readUntilBodyOpens(e){let t=0,s=0,i="";for(;!e.finished();e.move()){const n=e.currentChar();if(M(n))i+=this.readString(e,n);else{if("("===n)s++;else if(")"===n)s--;else if("{"===n){if(!t&&!s)break;t++}else if("}"===n&&(t--,t<0))throw new N(r.UNEXPECTED_SYMBOL,e.absPos(),n);i+=n}}if(i)return i}_stepper(){if(this.steps++,this.steps>this.maxSteps)throw new N(r.PARSER_STEPS_EXCEEDED,source.absPos())}}}},t={},function s(r){var i=t[r];if(void 0!==i)return i.exports;var n=t[r]={exports:{}};return e[r](n,n.exports,s),n.exports}(337);var e,t}));
//# sourceMappingURL=ludolfc.js.map