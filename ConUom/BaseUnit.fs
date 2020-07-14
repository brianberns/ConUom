namespace ConUom

/// Orthogonal dimensions of measurment.
type BaseDimension =
    | Length
    | Mass
    | Time

/// A base unit used to measure a dimension.
type BaseUnit =
    {
        /// Base dimension measured by this base unit.
        BaseDimension : BaseDimension

        /// Name of this base unit.
        Name : string
    }

module BaseUnit =

    /// Creates a base unit.
    let create dim name =
        {
            BaseDimension = dim
            Name = name
        }
