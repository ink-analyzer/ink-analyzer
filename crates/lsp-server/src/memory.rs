use std::collections::{HashMap, HashSet};
use std::mem;

/// Store a copy of the open documents in memory and tracks documents with unprocessed changes.
pub struct Memory {
    docs: HashMap<String, Document>,
    changes: HashSet<String>,
}

impl Memory {
    /// Creates a new instance.
    pub fn new() -> Self {
        Self {
            docs: HashMap::new(),
            changes: HashSet::new(),
        }
    }

    /// Adds document.
    pub fn insert(&mut self, id: String, content: String, version: i32) {
        self.docs.insert(id.clone(), Document { content, version });
        self.changes.insert(id);
    }

    /// Retrieves document.
    pub fn get(&self, id: &str) -> Option<&Document> {
        self.docs.get(id)
    }

    /// Updates document.
    pub fn update(&mut self, id: &str, content: String, version: i32) -> bool {
        match self.docs.get_mut(id) {
            Some(doc) => {
                doc.content = content;
                doc.version = version;
                self.changes.insert(id.to_owned());
                true
            }
            None => false,
        }
    }

    /// Removes document.
    pub fn remove(&mut self, id: &str) -> Option<Document> {
        self.docs.remove(id).map(|doc| {
            self.changes.insert(id.to_owned());
            doc
        })
    }

    /// Retrieves the document identifiers for documents with unprocessed changes and clears the change tracker.
    pub fn take_changes(&mut self) -> Option<HashSet<String>> {
        (!self.changes.is_empty()).then(|| mem::take(&mut self.changes))
    }
}

/// Represents a text document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document {
    /// The contents of the text documents.
    pub content: String,
    /// The version of the text document as defined by the LSP client.
    pub version: i32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn memory_works() {
        // Creates memory instance.
        let mut memory = Memory::new();

        // Adds documents.
        memory.insert("1".to_owned(), "A".to_owned(), 0);
        memory.insert("2".to_owned(), "B".to_owned(), 0);
        memory.insert("3".to_owned(), "C".to_owned(), 0);
        // Verifies expected changes.
        assert_eq!(
            memory.take_changes(),
            Some(HashSet::from([
                "1".to_owned(),
                "2".to_owned(),
                "3".to_owned()
            ]))
        );
        // Verifies that previous call to take changes clears the changes.
        assert_eq!(memory.take_changes(), None);

        // Retrieves document and verifies contents.
        assert_eq!(
            memory.get("1"),
            Some(Document {
                content: "A".to_owned(),
                version: 0
            })
            .as_ref()
        );
        // Tries to retrieve non-existent document.
        assert_eq!(memory.get("0"), None);

        // Updates document and verifies updated contents.
        assert!(memory.update("1", "A1".to_owned(), 1));
        assert_eq!(
            memory.get("1"),
            Some(Document {
                content: "A1".to_owned(),
                version: 1
            })
            .as_ref()
        );
        // Verifies changes.
        assert_eq!(memory.take_changes(), Some(HashSet::from(["1".to_owned()])));
        // Tries to update non-existent document and verifies that update doesn't perform an insert and sets changes properly.
        assert!(!memory.update("0", "Missing".to_owned(), 1));
        assert_eq!(memory.get("0"), None);

        // Removes document and verifies memory after removal and that changes are properly set.
        assert!(memory.remove("1").is_some());
        assert_eq!(memory.get("1"), None);
        // Verifies changes.
        assert_eq!(memory.take_changes(), Some(HashSet::from(["1".to_owned()])));
        // Tries to remove non-existent document.
        assert!(memory.remove("0").is_none());
    }
}
