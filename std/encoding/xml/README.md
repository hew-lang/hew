# std::encoding::xml

XML parsing and serialization for Hew.

## API

| Function | Description |
|---|---|
| `xml.parse(s)` | Parse an XML string into a `Node` tree |
| `node.to_string()` | Serialize a node tree back to XML |
| `node.get_tag()` | Get the tag name of an element |
| `node.get_attribute(name)` | Get an attribute value (empty if missing) |
| `node.children_count()` | Number of child nodes |
| `node.get_child(i)` | Get child node by index |
| `node.get_text()` | Concatenated text content (recursive) |
| `node.is_element()` | 1 for elements, 0 for text nodes |
| `node.free()` | Release node resources |

## Example

```hew
import std::encoding::xml;

fn main() {
    let root = xml.parse("<book lang=\"en\"><title>Hew Guide</title></book>");
    println(root.get_tag());            // book
    println(root.get_attribute("lang")); // en
    let title = root.get_child(0);
    println(title.get_text());          // Hew Guide
    title.free();
    root.free();
}
```
