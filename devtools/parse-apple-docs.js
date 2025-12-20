// ==UserScript==
// @name        parse-apple-docs.js
// @description Parse Apple's documentation
// @match       https://developer.apple.com/documentation/*?language=objc
// @run-at      document-end
// ==/UserScript==

const parseRule = {
    "Class":          parseClass,
    "Framework":      parseFramework,
    "API Collection": parseAPICollection,
    "Enumeration":    parseEnumeration
};


function appleDocumentType () {
    const type = document.querySelector("div.documentation-hero .topictitle .eyebrow");
    return type ? type.innerHTML.trim() : null;
}

function parseClass () {
    if (appleDocumentType() != "Class") {
        return "";
    }

    let name, desc;
    const nameNode = document.querySelector("div.documentation-hero .topictitle h1 span");
    const descNode = document.querySelector("div.documentation-hero .abstract.content");

    if (nameNode && descNode) {
        name = nameNode.innerText.trim();
        desc = descNode.innerText.trim();
    } else {
        return "";
    }

    let result = `
(doc-objc-class "${name}"
  "${desc}"
  "see ${window.location.href}")
`

    return result;
}

function parseFramework () {
    if (appleDocumentType() != "Framework") {
        return "";
    }

    let name, desc;
    const nameNode = document.querySelector("div.documentation-hero .topictitle h1 span");
    const descNode = document.querySelector("div.documentation-hero .abstract.content");

    if (nameNode && descNode) {
        name = nameNode.innerHTML.trim();
        desc = descNode.innerHTML.trim();
    } else {
        return "";
    }

    let result = `;;;; ${name.toLowerCase()}.lisp --- ObjC bindings for ${name} Framework
;; ${desc}
;; ${window.location.href}

(uiop:define-package #:coca.${name.toLowerCase()}
  (:use :cl :coca.objc)
  (:export ))

(in-package :coca.${name.toLowerCase()})
`;

    document.querySelectorAll(".section-content div.link-block").forEach(topic => {
        const name = topic.querySelector("a");
        const abst = topic.querySelector("div.abstract");
        if (name && abst) {
            result = result + `

;;;; ${name.innerText.trim()}
;; ${abst.innerText.trim()}
;; see ${name.href}
`;

        }
    });

    result += `
;;;; ${name.toLowerCase()}.lisp ends here`

    return result;
}

function parseAPICollection() {
    if (appleDocumentType() != "API Collection") {
        return "";
    }

    let result = "";
    document.querySelectorAll("div.contenttable-section").forEach(content => {
        const title = content.querySelector("a.header-anchor");
        if (title) {
            result = result + `
;;; ${title.innerText.trim()}
`;
            content.querySelectorAll("div.link-block.topic").forEach(topic => {
                const name = topic.querySelector("a");
                const abst = topic.querySelector("div.content");
                if (name && abst) {
                    result = result + `
(doc-objc-class "${name.innerText.trim()}"
  "${abst.innerText.trim()}"
  "see ${name.href}")
`;
                }
            })
        }
    });
    return result;
}

function parseEnumeration () {
    let enumName;
    const nameNode = document.querySelector("div.documentation-hero .topictitle h1 span");

    if (nameNode) {
        enumName = nameNode.innerHTML.trim();
    } else {
        return "";
    }


    let objcSrc = "";
    document.querySelectorAll("section:nth-child(2) div.link-block").forEach(name => {
        const e = name.querySelector('a');
        const d = name.querySelector('div.abstract');

        if (e && d) {
            const name = e.innerText.trim();
            objcSrc += `
NSLog(@"${name.replace(enumName, "")} %lu \\"${d.innerText.trim()}\\"", ${name});`
        }
    })
    return objcSrc;
}

function getButton () {
    let button = document.querySelector('#parseAppleDocument');
    if (button) return button;

    button = document.createElement('button');
    button.id = "parseAppleDocument";
    button.innerHTML = 'ParseMe';

    Object.assign(button.style, {
        position: 'fixed',
        bottom: '20px',
        right: '20px',
        zIndex: '9999',
        padding: '10px 20px',
        backgroundColor: '#0071e3',
        color: 'white',
        border: 'none',
        borderRadius: '8px',
        cursor: 'pointer',
        boxShadow: '0 2px 10px rgba(0,0,0,0.2)',
        fontWeight: 'bold'
    });

    button.addEventListener('click', () => {
        const type   = appleDocumentType();
        const action = parseRule[type];
        if (action) {
            const result = action();
            navigator.clipboard.writeText(result);
            alert("Copied to clipboard");
        }
    });

    document.body.appendChild(button);
    return button;
}

const observer = new MutationObserver((mutations, obs) => {
    const button = getButton();
    if (parseRule[appleDocumentType()]) {
        button.style.display = 'block';
    } else {
        button.style.display = 'none';
    }
});

// Watch the body for changes in the subtree
observer.observe(document.body, {
    childList: true,
    subtree: true
});
