package org.jetbrains.dukat.idlLowerings

import org.jetbrains.dukat.astCommon.IdentifierEntity
import org.jetbrains.dukat.astCommon.NameEntity
import org.jetbrains.dukat.astModel.ClassModel
import org.jetbrains.dukat.astModel.DocumentationCommentModel
import org.jetbrains.dukat.astModel.InterfaceModel
import org.jetbrains.dukat.astModel.SourceSetModel
import org.jetbrains.dukat.astModel.TopLevelModel
import org.jetbrains.dukat.translatorString.translate

fun TopLevelModel.addKDocs(): TopLevelModel {
    val documentation =
            "Exposes the JavaScript [${name.translate()}](https://developer.mozilla.org/en/docs/Web/API/${name.translate()}) to Kotlin"
    return when (this) {
        is ClassModel -> if (name in classesToBeDocumented) {
            copy(comment = DocumentationCommentModel(documentation))
        } else {
            this
        }
        is InterfaceModel -> if (name in classesToBeDocumented) {
            copy(comment = DocumentationCommentModel(documentation))
        } else {
            this
        }
        else -> this
    }
}

fun SourceSetModel.addKDocs(): SourceSetModel {
    return copy(sources = sources.map {
        it.copy(root = it.root.copy(declarations = it.root.declarations.map { topLevelModel ->
            topLevelModel.addKDocs()
        }))
    })
}

val classesToBeDocumented: Set<NameEntity> = listOf(
        "CSSStyleDeclaration",
        "StyleSheet",
        "CSSStyleSheet",
        "StyleSheetList",
        "LinkStyle",
        "CSSRuleList",
        "CSSRule",
        "CSSStyleRule",
        "CSSGroupingRule",
        "CSSMediaRule",
        "CSSPageRule",
        "CSSNamespaceRule",
        "CSS",
        "MediaStream",
        "MediaStreamTrack",
        "MediaTrackSupportedConstraints",
        "MediaTrackConstraints",
        "MediaTrackSettings",
        "MediaStreamTrackEvent",
        "MediaDevices",
        "MediaDeviceInfo",
        "MediaStreamConstraints",
        "DoubleRange",
        "ConstrainBooleanParameters",
        "ConstrainDOMStringParameters",
        "PointerEvent",
        "URL",
        "URLSearchParams",
        "SVGElement",
        "SVGGraphicsElement",
        "SVGGeometryElement",
        "SVGNumber",
        "SVGLength",
        "SVGAngle",
        "SVGNumberList",
        "SVGLengthList",
        "SVGAnimatedBoolean",
        "SVGAnimatedEnumeration",
        "SVGAnimatedInteger",
        "SVGAnimatedNumber",
        "SVGAnimatedLength",
        "SVGAnimatedAngle",
        "SVGAnimatedString",
        "SVGAnimatedRect",
        "SVGAnimatedNumberList",
        "SVGAnimatedLengthList",
        "SVGStringList",
        "SVGUnitTypes",
        "SVGTests",
        "SVGZoomAndPan",
        "SVGURIReference",
        "SVGSVGElement",
        "SVGGElement",
        "SVGDefsElement",
        "SVGDescElement",
        "SVGMetadataElement",
        "SVGTitleElement",
        "SVGSymbolElement",
        "SVGUseElement",
        "SVGSwitchElement",
        "SVGStyleElement",
        "SVGTransform",
        "SVGTransformList",
        "SVGAnimatedTransformList",
        "SVGPreserveAspectRatio",
        "SVGAnimatedPreserveAspectRatio",
        "SVGPathElement",
        "SVGRectElement",
        "SVGCircleElement",
        "SVGEllipseElement",
        "SVGLineElement",
        "SVGMeshElement",
        "SVGAnimatedPoints",
        "SVGPolylineElement",
        "SVGPolygonElement",
        "SVGTextContentElement",
        "SVGTextPositioningElement",
        "SVGTextElement",
        "SVGTSpanElement",
        "SVGTextPathElement",
        "SVGImageElement",
        "SVGForeignObjectElement",
        "SVGSolidcolorElement",
        "SVGGradientElement",
        "SVGLinearGradientElement",
        "SVGRadialGradientElement",
        "SVGStopElement",
        "SVGPatternElement",
        "SVGCursorElement",
        "SVGScriptElement",
        "SVGAElement",
        "SVGViewElement",
        "SVGClipPathElement",
        "SVGMaskElement",
        "XMLHttpRequestEventTarget",
        "XMLHttpRequest",
        "FormData",
        "ProgressEvent",
        "UIEvent",
        "FocusEvent",
        "MouseEvent",
        "WheelEvent",
        "InputEvent",
        "KeyboardEvent",
        "CompositionEvent",
        "Event",
        "EventTarget",
        "EventListener",
        "DOMParser",
        "XMLSerializer",
        "Notification",
        "NotificationEvent",
        "Navigator",
        "Document",
        "Window",
        "HTMLFormControlsCollection",
        "RadioNodeList",
        "HTMLOptionsCollection",
        "HTMLElement",
        "HTMLUnknownElement",
        "DOMStringMap",
        "HTMLHtmlElement",
        "HTMLHeadElement",
        "HTMLTitleElement",
        "HTMLBaseElement",
        "HTMLLinkElement",
        "HTMLMetaElement",
        "HTMLStyleElement",
        "HTMLBodyElement",
        "HTMLHeadingElement",
        "HTMLParagraphElement",
        "HTMLHRElement",
        "HTMLPreElement",
        "HTMLQuoteElement",
        "HTMLOListElement",
        "HTMLUListElement",
        "HTMLLIElement",
        "HTMLDListElement",
        "HTMLDivElement",
        "HTMLAnchorElement",
        "HTMLDataElement",
        "HTMLTimeElement",
        "HTMLSpanElement",
        "HTMLBRElement",
        "HTMLHyperlinkElementUtils",
        "HTMLModElement",
        "HTMLPictureElement",
        "HTMLSourceElement",
        "HTMLImageElement",
        "Image",
        "HTMLIFrameElement",
        "HTMLEmbedElement",
        "HTMLObjectElement",
        "HTMLParamElement",
        "HTMLVideoElement",
        "HTMLAudioElement",
        "HTMLTrackElement",
        "HTMLMediaElement",
        "MediaError",
        "AudioTrackList",
        "AudioTrack",
        "VideoTrackList",
        "VideoTrack",
        "TextTrack",
        "TextTrackCue",
        "TimeRanges",
        "TrackEvent",
        "HTMLMapElement",
        "HTMLAreaElement",
        "HTMLTableElement",
        "HTMLTableCaptionElement",
        "HTMLTableColElement",
        "HTMLTableSectionElement",
        "HTMLTableRowElement",
        "HTMLTableCellElement",
        "HTMLFormElement",
        "HTMLLabelElement",
        "HTMLInputElement",
        "HTMLButtonElement",
        "HTMLSelectElement",
        "HTMLDataListElement",
        "HTMLOptGroupElement",
        "HTMLOptionElement",
        "Option",
        "HTMLTextAreaElement",
        "HTMLKeygenElement",
        "HTMLOutputElement",
        "HTMLProgressElement",
        "HTMLMeterElement",
        "HTMLFieldSetElement",
        "HTMLLegendElement",
        "ValidityState",
        "HTMLDetailsElement",
        "HTMLDialogElement",
        "HTMLScriptElement",
        "HTMLTemplateElement",
        "HTMLSlotElement",
        "HTMLCanvasElement",
        "CanvasRenderingContext2D",
        "CanvasGradient",
        "CanvasPattern",
        "TextMetrics",
        "ImageData",
        "Path2D",
        "Touch",
        "ImageBitmapRenderingContext",
        "CustomElementRegistry",
        "DataTransfer",
        "DataTransferItemList",
        "DataTransferItem",
        "DragEvent",
        "History",
        "Location",
        "PopStateEvent",
        "HashChangeEvent",
        "PageTransitionEvent",
        "BeforeUnloadEvent",
        "NavigatorOnLine",
        "ErrorEvent",
        "PromiseRejectionEvent",
        "GlobalEventHandlers",
        "WindowEventHandlers",
        "WindowOrWorkerGlobalScope",
        "NavigatorID",
        "NavigatorLanguage",
        "NavigatorPlugins",
        "PluginArray",
        "MimeTypeArray",
        "Plugin",
        "MimeType",
        "ImageBitmap",
        "MessageEvent",
        "EventSource",
        "WebSocket",
        "CloseEvent",
        "MessageChannel",
        "MessagePort",
        "BroadcastChannel",
        "WorkerGlobalScope",
        "DedicatedWorkerGlobalScope",
        "SharedWorkerGlobalScope",
        "AbstractWorker",
        "Worker",
        "SharedWorker",
        "NavigatorConcurrentHardware",
        "WorkerNavigator",
        "WorkerLocation",
        "Storage",
        "WindowSessionStorage",
        "WindowLocalStorage",
        "StorageEvent",
        "HTMLMarqueeElement",
        "HTMLFrameSetElement",
        "HTMLFontElement",
        "CustomEvent",
        "DocumentOrShadowRoot",
        "ParentNode",
        "NonDocumentTypeChildNode",
        "ChildNode",
        "Slotable",
        "NodeList",
        "HTMLCollection",
        "MutationObserver",
        "MutationObserverInit",
        "MutationRecord",
        "Node",
        "XMLDocument",
        "DOMImplementation",
        "DocumentType",
        "DocumentFragment",
        "ShadowRoot",
        "Element",
        "NamedNodeMap",
        "Attr",
        "CharacterData",
        "Text",
        "CDATASection",
        "ProcessingInstruction",
        "Comment",
        "Range",
        "NodeIterator",
        "TreeWalker",
        "NodeFilter",
        "DOMTokenList",
        "DOMPointReadOnly",
        "DOMPoint",
        "DOMPointInit",
        "DOMRect",
        "DOMRectReadOnly",
        "DOMQuad",
        "DOMMatrixReadOnly",
        "DOMMatrix",
        "ScrollToOptions",
        "MediaQueryList",
        "MediaQueryListEvent",
        "Screen",
        "CaretPosition",
        "GeometryUtils",
        "Performance",
        "PerformanceTiming",
        "PerformanceNavigation",
        "ClipboardEvent",
        "Clipboard",
        "Headers",
        "Body",
        "Request",
        "Response",
        "WebGLBuffer",
        "WebGLFramebuffer",
        "WebGLProgram",
        "WebGLRenderbuffer",
        "WebGLShader",
        "WebGLTexture",
        "WebGLUniformLocation",
        "WebGLActiveInfo",
        "WebGLShaderPrecisionFormat",
        "WebGLRenderingContext",
        "WebGLContextEvent",
        "ArrayBuffer",
        "ArrayBufferView",
        "Int8Array",
        "Uint8Array",
        "Uint8ClampedArray",
        "Int16Array",
        "Uint16Array",
        "Int32Array",
        "Uint32Array",
        "Float32Array",
        "Float64Array",
        "DataView",
        "ServiceWorkerRegistration",
        "ServiceWorkerGlobalScope",
        "ServiceWorker",
        "ServiceWorkerContainer",
        "ServiceWorkerMessageEvent",
        "Client",
        "WindowClient",
        "Clients",
        "ExtendableEvent",
        "InstallEvent",
        "FetchEvent",
        "ExtendableMessageEvent",
        "Cache",
        "CacheStorage",
        "Blob",
        "File",
        "FileList",
        "FileReader",
        "FileReaderSync"
).map { IdentifierEntity(it) }.toSet()