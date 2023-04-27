"use strict";(self.webpackChunkcddl_codegen_documentation=self.webpackChunkcddl_codegen_documentation||[]).push([[554],{3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>f});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=r.createContext({}),s=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},c=function(e){var t=s(e.components);return r.createElement(p.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,p=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),u=s(n),m=a,f=u["".concat(p,".").concat(m)]||u[m]||d[m]||i;return n?r.createElement(f,o(o({ref:t},c),{},{components:n})):r.createElement(f,o({ref:t},c))}));function f(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=m;var l={};for(var p in t)hasOwnProperty.call(t,p)&&(l[p]=t[p]);l.originalType=e,l[u]="string"==typeof e?e:a,o[1]=l;for(var s=2;s<i;s++)o[s]=n[s];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},5992:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>o,default:()=>d,frontMatter:()=>i,metadata:()=>l,toc:()=>s});var r=n(7462),a=(n(7294),n(3905));const i={sidebar_position:4},o="Output format",l={unversionedId:"output_format",id:"output_format",title:"Output format",description:"- Inside of the output directly the tool always produces a rust/ directory (including Cargo.toml, etc).",source:"@site/docs/output_format.mdx",sourceDirName:".",slug:"/output_format",permalink:"/output_format",draft:!1,tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"tutorialSidebar",previous:{title:"Current capacities",permalink:"/current_capacities"},next:{title:"Wasm Differences",permalink:"/wasm_differences"}},p={},s=[],c={toc:s},u="wrapper";function d(e){let{components:t,...i}=e;return(0,a.kt)(u,(0,r.Z)({},c,i,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"output-format"},"Output format"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Inside of the output directly the tool always produces a ",(0,a.kt)("inlineCode",{parentName:"li"},"rust/")," directory (including Cargo.toml, etc). "),(0,a.kt)("li",{parentName:"ul"},"Unless we pass in ",(0,a.kt)("inlineCode",{parentName:"li"},"--wasm=false")," the tool also generates a corresponding ",(0,a.kt)("inlineCode",{parentName:"li"},"wasm/")," directory."),(0,a.kt)("li",{parentName:"ul"},"The default format for ",(0,a.kt)("inlineCode",{parentName:"li"},"rust/")," is to have a ",(0,a.kt)("inlineCode",{parentName:"li"},"lib.rs")," containing the structs and ",(0,a.kt)("inlineCode",{parentName:"li"},"serialization.rs")," containing their (de)serialization implementations/corresponding types."),(0,a.kt)("li",{parentName:"ul"},"The ",(0,a.kt)("inlineCode",{parentName:"li"},"wasm/")," directory is full of wasm_bindgen-annotated wrappers all in ",(0,a.kt)("inlineCode",{parentName:"li"},"lib.rs")," for the corresponding rust-use-only structs in ",(0,a.kt)("inlineCode",{parentName:"li"},"rust/")," and can be compiled for WASM builds by running ",(0,a.kt)("inlineCode",{parentName:"li"},"wasm-pack build")," on it.")),(0,a.kt)("p",null,(0,a.kt)("strong",{parentName:"p"},"Example Output")),(0,a.kt)("p",null,(0,a.kt)("img",{src:n(3785).Z,width:"415",height:"486"})),(0,a.kt)("admonition",{type:"note"},(0,a.kt)("p",{parentName:"admonition"},"The output format can change slightly depending on certain command line flags:")),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"--wasm=false")),(0,a.kt)("p",null,(0,a.kt)("img",{src:n(9575).Z,width:"385",height:"274"})),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"--preserve-encodings=true")),(0,a.kt)("p",null,(0,a.kt)("img",{src:n(374).Z,width:"383",height:"446"})),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"--json-schema-export true")),(0,a.kt)("p",null,(0,a.kt)("img",{src:n(8183).Z,width:"374",height:"400"})),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"--package-json true --json-schema-export true")),(0,a.kt)("p",null,(0,a.kt)("img",{src:n(332).Z,width:"372",height:"277"})))}d.isMDXComponent=!0},3785:(e,t,n)=>{n.d(t,{Z:()=>r});const r=n.p+"assets/images/output-81571003543ea17a552986eb68e06d71.png"},9575:(e,t,n)=>{n.d(t,{Z:()=>r});const r=n.p+"assets/images/output2-62b1d94d4e5f3ff0a6a2c18bd5b46ce1.png"},374:(e,t,n)=>{n.d(t,{Z:()=>r});const r=n.p+"assets/images/output3-b92905b6b0bb0b7ded7cb40758b241d4.png"},8183:(e,t,n)=>{n.d(t,{Z:()=>r});const r=n.p+"assets/images/output4-0e1dd4ffe08346854c5ab738cc6f3527.png"},332:(e,t,n)=>{n.d(t,{Z:()=>r});const r=n.p+"assets/images/output5-07ada9e131459f69389ca27cc90a5e7d.png"}}]);