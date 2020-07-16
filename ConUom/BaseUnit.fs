namespace ConUom

/// A base unit used to measure a dimension.
type BaseUnit =
    {
        /// Dimension measured by this base unit.
        Dimension : string

        /// Name of this base unit.
        Name : string
    }

module BaseUnit =

    /// Creates a base unit.
    let create dim name =
        {
            Dimension = dim
            Name = name
        }
