using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// ResourceKind
    ///
    /// Use this with the resource path functions to get the path to a
    /// given resource. Using these functions ensures that your resource
    /// paths are correct across platforms
    /// </summary>
    public enum ResourceKind
    {
        /// <summary>
        /// Indicates a Font Resource
        /// </summary>
        FontResource,
        /// <summary>
        /// Indicates a Image Resource
        /// </summary>
        ImageResource,
        /// <summary>
        /// Indicates a Sound Resource
        /// </summary>
        SoundResource,
        /// <summary>
        /// Indicates a Map Resource
        /// </summary>
        MapResource,
        /// <summary>
        /// Other resouces, located directly in the resource folder
        /// </summary>
        OtherResource,
        /// <summary>
        /// Indicates a No Resource
        /// </summary>
        None
    }
}
